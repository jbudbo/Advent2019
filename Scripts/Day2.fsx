open System.IO

let readData (path:string) =  
    File.ReadAllText(path).Split ',' 
    |> Array.map int
    |> Array.toList

let part1 (dataFile:string) =
    let cleanData = readData dataFile

    let execOp l =
        match l with
        | 1::[x;y;i] -> printfn "Add %i + %i in %i" x y i// cleanData.[i] = cleanData.[x] + cleanData.[y]
        | 2::[x;y;i] -> printfn "Mult %i + %i in %i" x y i //cleanData.[i] = cleanData.[x] * cleanData.[y]
        | 99::_ -> printfn "STOP"
        | _ -> printfn "ERR"

    cleanData
    |> List.chunkBySize 4
    |> List.iter (fun x -> execOp x)