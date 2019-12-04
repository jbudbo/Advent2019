let candidates = [|387638..919123|]

let intToInts i = 
    // I think this could be done MUCH better but this splits the ints and keeps them in order
    [|
        i / 100000;
        i % 100000 / 10000; 
        i % 10000 / 1000; 
        i % 1000 / 100;
        i % 100 / 10;
        i % 10;
    |]

let qualifies (ints:int[]) :bool =
    let windows = Array.windowed 2 ints

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
|> Array.where qualifies
|> Array.where atLeastOnePair
|> Array.length 