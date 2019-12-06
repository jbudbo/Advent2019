open System.IO

let readOrbits file = 
    File.ReadAllLines file
    |> Array.map (fun l -> l.Split(')') |> (fun [|x;y;|] -> (y,x)))
    //|> Map.ofArray

readOrbits "inputs/Day6"
|> Array.sortBy fst