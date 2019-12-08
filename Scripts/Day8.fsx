open System.IO

let inline toInt c = int c - int '0'

let readData (path:string) =  
    File.ReadAllText(path) 
    |> Seq.map toInt

let getLayers x y = 
    // readData "inputs/Day8" 
    "0222112222120000" |> Seq.map toInt
    |> Seq.chunkBySize (x * y)

let part1 viewX viewY =
    getLayers viewX viewY
    |> Seq.map (fun x -> x |> Seq.countBy id |> Seq.sortBy fst)
    |> Seq.sortBy (fun x -> x |> Seq.head |> snd)
    |> Seq.head
    |> Seq.where (fun x -> fst x > 0)
    |> Seq.map snd
    |> Seq.fold (fun x i -> x * i) 1

let part2 viewX viewY =
    getLayers viewX viewY

let flattener = 
    let smallest x y = if x < y then x else y
    [ smallest; smallest; smallest ]

let apply arr = Seq.map2 (fun f x -> f x) flattener arr

part2 2 2 |> Seq.toArray |> apply |> Seq.toArray

// part1 25 6