module App

let factors (x : int) = 
  let inc = function 2 -> 3 | x -> x + 2
  let sqrt = float >> System.Math.Sqrt >> int
  let pack y = (y, sqrt y)
  let rec fact fs f = function
    | 0,_ | 1,_          -> fs
    | y,yrt when yrt < f -> y :: fs
    | y,yrt              -> match y % f with
                            | 0 -> fact (f :: fs) f (y / f |> pack)
                            | _ -> fact fs (inc f) (y,yrt) 
  x |> System.Math.Abs |> pack |> fact [] 2 

let sumByFactors : int seq -> (int * int) seq = 
  Seq.map (fun x -> x |> factors |> Seq.distinct |> Seq.zip (Seq.initInfinite (fun _ -> x))) 
  >> Seq.concat 
  >> Seq.groupBy snd 
  >> Seq.sortBy fst 
  >> Seq.map (fun (x, xs) -> x, xs |> Seq.sumBy fst)
