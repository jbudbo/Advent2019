let inline fst (a,_,_) = a;
let inline snd (_,a,_) = a;
let inline thrd (_,_,a) = a;

let inline onEach actor s = seq {
    for i in s do
        yield! actor i
}

let mockData = seq {
    yield (1,0,2)
    yield (2,-10,-7)
    yield (4,-8,8)
    yield (3,5,1)
}

let processData (d:seq<'a*'b*'c>) =
    let local = Seq.cache d

    local 
    |> Seq.map fst
    |> fun h::t ->
        
    

mockData |> processData
|> Seq.toArray