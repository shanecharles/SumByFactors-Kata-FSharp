#r "packages/FAKE/tools/fakelib.dll"

open Fake.Testing.XUnit2
open Fake

let buildDir = "./bin/build/"
let testDir = "./bin/tests/"

Target "Clean" (fun _ ->
    CleanDirs [buildDir; testDir]
)

Target "BuildApp" (fun _ ->
    !! "src/app/**/*.fsproj"
      |> MSBuildRelease buildDir "Build"
      |> Log "AppBuild-Output: "
)

Target "BuildTest" (fun _ ->
    !! "src/tests/**/*.fsproj"
      |> MSBuildDebug testDir "Build"
      |> Log "TestBuild-Output: "
)

Target "RunTests" (fun _ ->
    !! (testDir @@ "/*.Tests.dll")
        |> xUnit (fun p -> {p with 
                                ShadowCopy = false;
                                ToolPath = "packages/xunit.runners/tools/xunit.console.clr4.exe"})
)

Target "Default" (fun _ ->
    trace "Build from fake"
)

"Clean"
  ==> "BuildTest"
  ==> "RunTests"
  ==> "BuildApp"
  ==> "Default"

RunTargetOrDefault "Default"
