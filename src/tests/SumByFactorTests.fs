namespace TestRunner

open Xunit
open Xunit.Extensions
open FsCheck
open FsCheck.Xunit
open App

type NonZeroOrOne = 
  static member Value () = Arb.Default.Int32 () 
                           |> Arb.filter (fun x -> System.Int32.MinValue <> x && x < -1 || 1 < x)

type NonZeroOrOnePropertyAttribute () =
  inherit PropertyAttribute (Arbitrary = [| typeof<NonZeroOrOne> |])

module Tests =
  [<NonZeroOrOneProperty>]
  let ``Factors for a non zero or one number should produce the absolute value of original.`` (x : int) =
    let expected = System.Math.Abs x
    let actual = x |> factors |> Seq.fold (*) 1
    expected = actual

  [<NonZeroOrOneProperty>]
  let ``Random non zero or one should not be zero or one.`` (x : int)  =
    x < -1 || 1 < x

  [<NonZeroOrOneProperty>]
  let ``Random number should not equal Int32.MinValue.`` (x : int) =
    System.Int32.MinValue <> x

  [<Theory>]
  [<InlineData(0)>]
  [<InlineData(1)>]
  [<InlineData(-1)>]
  let ``The factors for 0, 1, and -1 should be empty.`` edge =
    [] = factors edge

  [<Theory>]
  [<InlineData(0)>]
  [<InlineData(1)>]
  [<InlineData(-1)>]
  let ``Given a sequence with 0, 1, or -1, sumByFactors should return an empty sequence.`` edge =
    [edge] |> sumByFactors |> Seq.isEmpty

  [<Fact>]
  let ``Sequence consisting of 15, 30, and -45 should return [(2,30); (3,0); (5,0)].`` () =
    let expected = [(2,30); (3,0); (5,0)]
    let actual = [15; 30; -45] |> sumByFactors |> Seq.toList
    expected = actual