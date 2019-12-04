open System
open System.IO

let split = 
    let splitBy (c:char) (line:string) :string[] = line.Split(c)
    splitBy ','

let readData (path: string) = 
    File.ReadAllLines(path)
    |> Array.map split // Don't use collect as each line represents a wire

let processWire (wire:string[]) =
    0

readData "Scripts/inputs/Day3"
|> Array.map processWire

let moveOnGrid (x,y) s =
    match s with
    | 'R'::u -> new String(u) |> int |> (fun i -> (x + i, y))
    | 'U'::u -> new String(u) |> int |> (fun i -> (x, y + i))
    | 'L'::u -> new String(u) |> int |> (fun i -> (x - i, y))
    | 'D'::u -> new String(u) |> int |> (fun i -> (x, y - i))
    | _ -> (x,y)

[|"R997"; "U849"; "R349"; "U641";|]
|> Array.fold moveOnGrid (0,0)