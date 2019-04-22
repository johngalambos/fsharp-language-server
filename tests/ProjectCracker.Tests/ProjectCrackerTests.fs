module ProjectCrackerTests

open ProjectCracker
open ProjectCrackerTestsCommon
open LSP.Log
open System
open System.IO
open System.Text.RegularExpressions
open System.Diagnostics
open NUnit.Framework
open System.Collections.Concurrent
open FSharp.Compiler.ErrorLogger

[<SetUp>]
let setup() = 
    LSP.Log.diagnosticsLog := stdout

let containsFileName(name: string, files: FileInfo list) = 
  let test(f: FileInfo) = name = f.Name
  List.exists test files


let crack fsproj = ProjectCracker.load (ConcurrentDictionary<_,_>()) fsproj

let getTargetDllFromOpts (options:FSharp.Compiler.SourceCodeServices.FSharpProjectOptions) = 
    match options.ExtraProjectInfo with
    | Some opt -> 
        let extraData = unbox<ExtraProjectInfoData>(opt)
        match extraData.ProjectSdkType with 
        | ProjectCracker.ProjectSdkType.DotnetSdk sdkProj -> 
            FileInfo(sdkProj.TargetPath).Name
    | None -> failwith "Expected target"

let getReferences (otherOptions: string []) =
    [ for o in otherOptions do
        if o.StartsWith("-r:") then
            yield FileInfo(o.[3..]).Name ]

[<Test>]
let ``crack a project file``() = 
    let fsproj = Path.Combine [|projectRoot.FullName; "sample"; "MainProject"; "MainProject.fsproj"|] |> FileInfo 
    let crackResult = crack fsproj.FullName
    match crackResult with 
    | Result.Ok (options, compileFiles, log) -> 
        // Direct project reference
        // printfn "%A" options
        let projectNames = [for f in options.ReferencedProjects 
                                do yield FileInfo((snd f).ProjectFileName).Name]
        if not(List.contains "DependsOn.fsproj" projectNames) then
            Assert.Fail(sprintf "No DependsOn.fsproj in %A" projectNames)
        // Transitive dependency
        if not(List.contains "IndirectDep.fsproj" projectNames) then
            Assert.Fail(sprintf "No IndirectDep.fsproj in %A" projectNames)
        // Output dll
        Assert.AreEqual("MainProject.dll", getTargetDllFromOpts options)
    | Result.Error (_) -> Assert.Fail "Error cracking project"

[<Test>]
let ``crack a project file with case insensitive package references`` () =
    let fsproj = Path.Combine [|projectRoot.FullName; "sample"; "HasPackageReference"; "HasPackageReference.fsproj" |] |> FileInfo
    let crackResult = crack (fsproj.FullName)
    match crackResult with 
    | Result.Ok (options, compileFiles, log) ->
        let references = getReferences(options.OtherOptions)
        CollectionAssert.Contains(references, "Logary.dll")
    | Result.Error (_) -> Assert.Fail "Error cracking project"


[<Test>]
let ``find compile sources``() = 
    let fsproj = Path.Combine [|projectRoot.FullName; "sample"; "IndirectDep"; "IndirectDep.fsproj"|] |> FileInfo 
    let crackResult = crack (fsproj.FullName)
    match crackResult with 
    | Result.Ok (options, compileFiles, log) ->
        let sourceNames = compileFiles |> List.map (fun p -> FileInfo(p).Name)
        CollectionAssert.Contains(sourceNames, "IndirectLibrary.fs")
    | Result.Error (_) -> Assert.Fail "Error cracking project"

[<Test>]
let ``find reference includes``() = 
    let fsproj = Path.Combine [|projectRoot.FullName; "sample"; "HasLocalDll"; "HasLocalDll.fsproj"|] |> FileInfo 
    let crackResult = crack (fsproj.FullName)
    match crackResult with 
    | Result.Ok (options, _, _) ->
        let references = getReferences(options.OtherOptions)
        CollectionAssert.Contains(references, "IndirectDep.dll")
    | Result.Error (_) -> Assert.Fail "Error cracking project"

[<Test>]
let ``find CSharp reference``() = 
    let fsproj = Path.Combine [|projectRoot.FullName; "sample"; "ReferenceCSharp"; "ReferenceCSharp.fsproj"|] |> FileInfo 
    let crackResult = crack (fsproj.FullName)
    match crackResult with 
    | Result.Ok (options, _, _) ->
        let references = getReferences(options.OtherOptions)
        CollectionAssert.Contains(references, "CSharpProject.dll")
    | Result.Error (_) -> Assert.Fail "Error cracking project"

[<Test>]
let ``find CSharp reference with modified AssemblyName``() = 
    let fsproj = Path.Combine [|projectRoot.FullName; "sample"; "ReferenceCSharp.AssemblyName"; "ReferenceCSharp.AssemblyName.fsproj"|] |> FileInfo 
    let crackResult = crack (fsproj.FullName)
    match crackResult with 
    | Result.Ok (options, _, _) ->
        let references = getReferences(options.OtherOptions)
        CollectionAssert.Contains(references, "CSharpProject.AssemblyName.Modified.dll")
    | Result.Error (_) -> Assert.Fail "Error cracking project"

[<Test>]
let ``resolve template params``() = 
    let fsproj = Path.Combine [|projectRoot.FullName; "sample"; "TemplateParams"; "TemplateParams.fsproj"|] |> FileInfo 
    let crackResult = crack (fsproj.FullName)
    match crackResult with 
    | Result.Ok (options, compileFiles, log) ->
        let expected = [
            Path.Combine([|projectRoot.FullName; "src"; "fsharp"; "QueueList.fs"|]); 
            Path.Combine([|projectRoot.FullName; "sample"; "TemplateParams"; "netstandard2.0"; "pars.fs"|])
        ]
        CollectionAssert.AreEquivalent(expected, compileFiles)
    | Result.Error (e) -> Assert.Fail (sprintf "Error cracking project: %s" e)

// Check that project.assets.json-based ProjectCrackerDS finds same .dlls as MSBuild

let clean(fsproj: FileInfo) = 
    let args = sprintf "clean %s" fsproj.FullName
    let info =
        ProcessStartInfo(
            UseShellExecute = false,
            FileName = "dotnet",
            Arguments = args
        )
    let p = Process.Start(info)
    p.WaitForExit()

let msbuild(fsproj: FileInfo): string list = 
    // Clean project so `dotnet build` actually generates output
    clean(fsproj)
    // Invoke `dotnet build`
    let args = sprintf "build %s -v d" fsproj.FullName
    let info =
        ProcessStartInfo(
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            FileName = "dotnet",
            Arguments = args
        )
    let p = Process.Start(info)
    // Collect all lines of stdout
    let lines = System.Collections.Generic.List<string>()
    p.OutputDataReceived.Add(fun args -> if args.Data <> null then lines.Add(args.Data))
    p.ErrorDataReceived.Add(fun args -> if args.Data <> null then dprintfn "Build: %A" args.Data)
    if not(p.Start()) then 
        failwithf "Failed dotnet %s" args
    p.BeginOutputReadLine()
    p.BeginErrorReadLine()
    p.WaitForExit()
    // Search for lines that start with '-r:'
    let references = System.Collections.Generic.List<string>()
    for line in lines do 
        if line.EndsWith("Task \"Fsc\"") then 
            references.Clear()
        if line.Trim().StartsWith("-r:") then 
            references.Add(line.Trim().Substring("-r:".Length))
    // Filter out project-to-project references, these are handled separately by ProjectCrackerDS
    [ for r in references do 
        if not(r.Contains("bin/Debug/netcoreapp2.0")) then 
            yield r ]
    
let cracker(fsproj: FileInfo): string list = 
    let crackResult = crack (fsproj.FullName)
    match crackResult with 
    | Result.Ok (options, compileFiles, log) ->
        [ for o in options.OtherOptions do
            if o.StartsWith("-r:") then
                yield o.[3..]]
    | Result.Error (e) ->
        failwith "Failed to crack project"
        
[<Test>]
let ``find package references in EmptyProject``() = 
    let fsproj = Path.Combine [|projectRoot.FullName; "sample"; "EmptyProject"; "EmptyProject.fsproj"|] |> FileInfo 
    CollectionAssert.AreEquivalent(msbuild(fsproj), cracker(fsproj))
        
[<Test>]
let ``find package references in FSharpKoans``() = 
    let fsproj = Path.Combine [|projectRoot.FullName; "sample"; "FSharpKoans.Core"; "FSharpKoans.Core.fsproj"|] |> FileInfo 
    CollectionAssert.AreEquivalent(msbuild(fsproj), cracker(fsproj))

[<Test>]
let ``error for unbuilt project``() = 
    let fsproj = Path.Combine [|projectRoot.FullName; "sample"; "NotBuilt"; "NotBuilt.fsproj"|] |> FileInfo 
    let crackResult = crack (fsproj.FullName)
    match crackResult with 
    | Result.Ok (options, compileFiles, log) ->
        Assert.Fail("Should have failed to crack unbuilt project")
    | Result.Error (e) -> StringAssert.Contains("not restored", e)
