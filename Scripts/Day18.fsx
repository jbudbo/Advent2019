open System.IO

type Leaf = { coord:int*int*char; east:Leaf option; west:Leaf option }

//   A to Z
let upperRange = [|97..122|]
//   a to z
let lowerRange = [|65..90|]

let inline horz (a,_,_) = a
let inline vrt (_,b,_) = b
let inline tok (_,_,c) = c

let readMapData (file:string) = seq { 
    let lines = File.ReadAllLines file

    let identify (index:int) (row:string) = seq {
        for i = 0 to row.Length - 1 do
            yield match row.[i] with
                  | '#' -> None
                  | c -> Some((index,i,c))
    }        

    for i = 0 to lines.Length - 1 do
        yield! identify i lines.[i]
}

let processMap map =
    let local = Seq.cache map

    let start :Leaf= {coord=Seq.find (fun coord -> tok coord = '@') local; east=None; west=None;}

    //  Define a tree of movement 
    let rec insertLeaf (tree:Leaf option) (coord:int*int*char) :Leaf =
        match tree with
        | Some t -> if vrt coord < vrt t.coord then { t with west=Some(insertLeaf t.west coord)}
                    else if vrt coord > vrt t.coord then { t with east=Some(insertLeaf t.east coord)}
                    else t
        | None -> {coord=coord; east=None; west=None}

    local |> Seq.fold (fun acc item -> Some (insertLeaf acc item)) (Some start)
    //start

readMapData "inputs/Day18" 
|> Seq.choose id // Can probably refactor this to use choose more wisely but I needed x,y coord...
|> processMap