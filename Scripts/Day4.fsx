let candidates = [|387638..919123|]

let intToInts =
    // Thanks Ted
    let tokenize (i:int) = if i = 0 then None else Some(i%10,i/10)
    let crack (i:int) = i |> Array.unfold tokenize |> Array.rev
    crack

let qualifies (ints:int[]) :bool =
    let windows = Array.windowed 2 ints

    //  Should look at linter warning here
    let hasPair (a:int[][]) :bool = a |> Array.exists (fun [|h;t|] -> h = t)
    let incremental (a:int[][]) :bool = a |> Array.forall (fun [|h;t|] -> h <= t)

    // Not a fan of this, seems we could/should curry
    incremental windows && hasPair windows

let atLeastOnePair (ints:int[]) :bool =
    ints 
    |> Array.groupBy id
    |> Array.where (fun (_,x) -> (Array.length x) = 2) 
    |> Array.length > 0
   

candidates
|> Array.map intToInts
|> Array.where qualifies // Part 1 would have an |> Array.length
|> Array.where atLeastOnePair
|> Array.length 