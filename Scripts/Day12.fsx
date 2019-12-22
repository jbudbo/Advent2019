let input = seq {
    // (x, y, z)
    yield (15, 1, 4) // Io
    yield (1, -10, -8) // Europa
    yield (-5, 4, 9) // Ganymede
    yield (4, 6, -2) // Callisto
}

let mock = seq {
    // (x, y, z)
    yield (-1, 0, 2) // Io
    yield (2, -10, -7) // Europa
    yield (4, -8, 8) // Ganymede
    yield (3, 5, -1) // Callisto
}

let inline fst (a,_,_) = a;
let inline snd (_,b,_) = b;
let inline thrd (_,_,c) = c;

let applyGravity (moons:seq<int*int*int>) :seq<int*int*int> =     
    let cache = Seq.cache moons

    //let onEach (actor:'a 'a -> unit) (start:'a[]) =
    //    actor 

    let calcDimension (dim:int[]) =
        dim

    let x = Seq.map fst cache
    let y = Seq.map snd cache
    let z = Seq.map thrd cache

    

    //Seq.zip3 x y z

mock
|> applyGravity
|> Seq.toArray