open System.IO

let (|Gate|Key|Origin|Empty|) (space:char) =
    match space with
    | '@' -> Origin
    | s when Array.exists ((=) s) [|'a'..'z'|] -> Key
    | s when Array.exists ((=) s) [|'A'..'Z'|] -> Gate
    | _ -> Empty

type Vector = {X:int; Y:int}
type Position = {Coord:Vector; Token:char}

let inline origin (p:Position) :bool = match p.Token with Origin -> true | _ -> false

let inline int (p:Position) =
    match p.Token with
    | Origin -> 0
    | Key -> 1
    | _ -> -1

let readMapData (file:string) = seq { 
    let lines = File.ReadAllLines file

    let identify (index:int) (row:string) = seq {
        for i = 0 to row.Length - 1 do
            yield match row.[i] with
                  | '#' -> None
                  | c -> Some({Coord={X=i;Y=index};Token=c})
    }        

    for i = 0 to lines.Length - 1 do
        yield! identify i lines.[i]
}

let processMap (map:seq<Position>) =
    let local = Seq.cache map
    
    let start = Seq.find origin local

    let rec move (inventory:char[]) (o:Position) =
        let eastVal = local |> Seq.where (fun p -> p.Coord.X > o.Coord.X) |> Seq.sumBy int
        let westVal = local |> Seq.where (fun p -> p.Coord.X < o.Coord.X) |> Seq.sumBy int

        printfn "origin %A east %A - west %A" o eastVal westVal

    move [||] start

    local //|> Seq.toArray// Seq.fold (fun acc item -> Some (insertLeaf acc item)) (Some start)
    //start

readMapData "inputs/Day18" 
|> Seq.choose id // Can probably refactor this to use choose more wisely but I needed x,y coord...
|> processMap