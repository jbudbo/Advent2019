open System.IO

let readInput = File.ReadAllLines "inputs/Day10" |> Array.map  Seq.toArray

for y in readInput do
    for x in y do
       printfn "%A" x