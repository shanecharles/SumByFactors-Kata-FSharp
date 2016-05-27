namespace TestRunner

open Xunit
open Xunit.Extensions
open App

module Tests =
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