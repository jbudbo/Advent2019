open System.IO

let readData (path:string) = File.ReadLines(path)
let calculateFuel (mass:int) = mass / 3 - 2
let rec calculateFuelDeep (mass:int) =
    let start = calculateFuel mass
    if start > 0 then
        start + (calculateFuelDeep start)
    else 
        0

let part1 (dataFile:string) =
    let massage (x:string) = int x |> calculateFuel
    readData dataFile
        |> Seq.sumBy massage

let part2 (dataFile:string) =
    let massage (x:string) = int x |> calculateFuelDeep
    readData dataFile
    |> Seq.sumBy massage