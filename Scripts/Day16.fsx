open System.IO

let inline toInt c = int c - int '0'

let inline onesPlace (i:int) = (abs i) % 10

let inline numberStream path = File.ReadAllText(path).ToCharArray() |> Array.map toInt

// Will likely need to permute offsets
let baseOffset :int[] = [|0; 1; 0; -1|]



[|1;2;3;4;5;6;7;8|] 