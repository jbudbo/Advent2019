open System.IO

let readData (path:string) =  
    File.ReadAllText(path).Split ',' 
    |> Array.map int

let compute (cleanData:int[]) =
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
            | None -> () //Burn
        
    chunki 0

    cleanData

let getResultFor noun verb =
    readData "inputs/Day2"
    |> fun (arr:int[]) ->
        arr.[1] <- noun
        arr.[2] <- verb
        arr
    |> compute
    |> Array.head

// Part1
getResultFor 12 2

// Part2
// I can do this so much better
let generator = seq {
    for x in [|0..99|] do
        for y in [|0..99|] do
            yield (x,y,getResultFor x y)
}

generator 
|> Seq.where (fun (_,_,t) -> t = 19690720) 
|> Seq.map (fun (noun,verb,_) -> 100 * noun + verb)
|> Seq.exactlyOne