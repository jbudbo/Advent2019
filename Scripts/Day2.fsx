open System.IO

let readData (path:string) =  
    File.ReadAllText(path).Split ',' 
    |> Array.map int

let part1 (dataFile:string) =
    let cleanData = readData dataFile

    //  Start puzzle
    cleanData.[1] <- 12
    cleanData.[2] <- 2

    let add x y i =
        cleanData.[i] <- cleanData.[x] + cleanData.[y]

    let mult x y i =
        cleanData.[i] <- cleanData.[x] * cleanData.[y]

    let execOp (l:int[]) =
        match Array.head l with
        | 1 -> Some(add l.[1] l.[2] l.[3])
        | 2 -> Some(mult l.[1] l.[2] l.[3])
        | 99 -> None
        | x -> failwithf "Unknown OP Code %i" x

    let rec chunki i =
        if i <= (cleanData.Length - 4) then
            match Array.sub cleanData i 4 |> execOp with
            | Some(_) -> chunki (i + 4)
            | None -> printf "DONE"
        
    chunki 0

    cleanData.[0]

part1 "inputs/Day2"