module Tests 

open FsUnit
open NUnit.Framework

[<TestFixture>]
type AccountTest() =
  [<Test>]
  member x.SimpleTest() = 
    1 |> should equal 1