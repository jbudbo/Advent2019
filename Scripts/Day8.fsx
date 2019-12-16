open System.IO

let inline toInt c = int c - int '0'

let readData (path:string) =  
    File.ReadAllText(path) 
    |> Seq.map toInt

let part1 viewX viewY =
    readData "Scripts/inputs/Day8" 
    |> Seq.chunkBySize (viewX * viewY)
    |> Seq.map (fun x -> x |> Seq.countBy id |> Seq.sortBy fst)
    |> Seq.sortBy (fun x -> x |> Seq.head |> snd)
    |> Seq.head
    |> Seq.where (fun x -> fst x > 0)
    |> Seq.map snd
    |> Seq.fold (*) 1

let part2 viewX viewY =
    readData "Scripts/inputs/Day8"
    |> Seq.chunkBySize (viewX * viewY)
    |> Seq.toArray
    |> Seq.fold (fun acc layer ->
        Array.map2 (fun a b -> 
            match a with
            | 2 -> b // 2 is transparent
            | _ -> a // If it's anything else take that
        ) acc layer
    ) (Array.create (viewX * viewY) 2)
    |> Array.chunkBySize viewX
    |> Array.map (fun acc -> acc |> Array.map (fun c -> 
        // Lets make this more readable
        match c with 
        | 0 -> '-' // Black
        | 1 -> '@' // White 
        | 2 -> ' ' // Transparent
        | _ -> ' '
        ))
    |> Array.map System.String
    |> Array.iter (fun s -> printfn "%s" s)

// part1 25 6

part2 25 6