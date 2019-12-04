let candidates = [|387638..919123|]

let rec intToInts i = 
    let work = seq {
        yield i % 10
        
        let next = i / 10

        if next > 0 then yield! intToInts next
    }
    Seq.toArray work

candidates
|> Array.map intToInts
|> Array.reduce