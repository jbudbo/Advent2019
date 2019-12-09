open System.IO

let inline sxth (_,_,_,_,_,x) = x

let runProgram phase input =
    let intToInts =
        let tokenize (i:int) = if i = 0 then None else Some(i%10,i/10)
        let crack (i:int) = i |> Array.unfold tokenize |> Array.rev
        crack

    let padLeft (arr:int[]) :int[] =
        let result = Array.zeroCreate 3
        arr |> Array.rev |> Array.iteri (fun acc a -> result.[acc] <- a)
        result

    File.ReadAllText("inputs/Day7").Split ','
    // "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5".Split ','
    |> Array.map int
    |> fun instructionSet ->
        let mutable output = 0
        let rec executeOnInput x =
            let op = instructionSet.[x] % 100
            let modes = instructionSet.[x] / 100 |> intToInts |> padLeft

            match op with
            | 1 -> // ADD
                let t :int[] = instructionSet.[(x+1)..x+3]
                let param1 = if modes.[0] = 0 then instructionSet.[t.[0]] else t.[0]
                let param2 = if modes.[1] = 0 then instructionSet.[t.[1]] else t.[1]
                instructionSet.[t.[2]] <- param1 + param2
                executeOnInput (x + 4)

            | 2 -> // MULTIPLY
                let t :int[] = instructionSet.[(x+1)..x+3]
                let param1 = if modes.[0] = 0 then instructionSet.[t.[0]] else t.[0]
                let param2 = if modes.[1] = 0 then instructionSet.[t.[1]] else t.[1]
                instructionSet.[t.[2]] <- param1 * param2
                executeOnInput (x+4)

            | 3 -> // INPUT
                let t :int = instructionSet.[x+1]
                instructionSet.[t] <- input
                executeOnInput (x+2)

            | 4 -> // OUTPUT
                let t :int = instructionSet.[x+1]
                output <- instructionSet.[t]
                // executeOnInput (x+2)

            | 5 -> // JUMP IF TRUE
                let t :int[] = instructionSet.[(x+1)..x+3]
                let param1 = if modes.[0] = 0 then instructionSet.[t.[0]] else t.[0]
                let param2 = if modes.[1] = 0 then instructionSet.[t.[1]] else t.[1]
                if param1 <> 0 then
                    executeOnInput (param2)
                else
                    executeOnInput (x + 3) 

            | 6 -> // JUMP IF FALSE
                let t :int[] = instructionSet.[(x+1)..x+3]
                let param1 = if modes.[0] = 0 then instructionSet.[t.[0]] else t.[0]
                let param2 = if modes.[1] = 0 then instructionSet.[t.[1]] else t.[1]
                if param1 = 0 then 
                    executeOnInput (param2)
                else
                    executeOnInput (x + 3) 

            | 7 -> // LESS THAN
                let t :int[] = instructionSet.[(x+1)..x+3]
                let param1 = if modes.[0] = 0 then instructionSet.[t.[0]] else t.[0]
                let param2 = if modes.[1] = 0 then instructionSet.[t.[1]] else t.[1]
                instructionSet.[t.[2]] <- if param1 < param2 then 1 else 0
                executeOnInput (x+4)

            | 8 -> // EQUALS
                let t :int[] = instructionSet.[(x+1)..x+3]
                let param1 = if modes.[0] = 0 then instructionSet.[t.[0]] else t.[0]
                let param2 = if modes.[1] = 0 then instructionSet.[t.[1]] else t.[1]
                instructionSet.[t.[2]] <- if param1 = param2 then 1 else 0
                executeOnInput (x+4)

            | 99 -> ()
            | x -> failwithf "Unexpected OP Code %i" x

        let rec executeOnPhase x =
            let op = instructionSet.[x] % 100
            let modes = instructionSet.[x] / 100 |> intToInts |> padLeft

            match op with
            | 1 -> // ADD
                let t :int[] = instructionSet.[(x+1)..x+3]
                let param1 = if modes.[0] = 0 then instructionSet.[t.[0]] else t.[0]
                let param2 = if modes.[1] = 0 then instructionSet.[t.[1]] else t.[1]
                instructionSet.[t.[2]] <- param1 + param2
                executeOnPhase (x + 4)

            | 2 -> // MULTIPLY
                let t :int[] = instructionSet.[(x+1)..x+3]
                let param1 = if modes.[0] = 0 then instructionSet.[t.[0]] else t.[0]
                let param2 = if modes.[1] = 0 then instructionSet.[t.[1]] else t.[1]
                instructionSet.[t.[2]] <- param1 * param2
                executeOnPhase (x+4)

            | 3 -> // INPUT
                let t :int = instructionSet.[x+1]
                instructionSet.[t] <- phase
                executeOnInput (x+2)

            | 4 -> // OUTPUT
                let t :int = instructionSet.[x+1]
                output <- instructionSet.[t]
                // executeOnPhase (x+2)

            | 5 -> // JUMP IF TRUE
                let t :int[] = instructionSet.[(x+1)..x+3]
                let param1 = if modes.[0] = 0 then instructionSet.[t.[0]] else t.[0]
                let param2 = if modes.[1] = 0 then instructionSet.[t.[1]] else t.[1]
                if param1 <> 0 then
                    executeOnPhase (param2)
                else
                    executeOnPhase (x + 3) 

            | 6 -> // JUMP IF FALSE
                let t :int[] = instructionSet.[(x+1)..x+3]
                let param1 = if modes.[0] = 0 then instructionSet.[t.[0]] else t.[0]
                let param2 = if modes.[1] = 0 then instructionSet.[t.[1]] else t.[1]
                if param1 = 0 then 
                    executeOnPhase (param2)
                else
                    executeOnPhase (x + 3) 

            | 7 -> // LESS THAN
                let t :int[] = instructionSet.[(x+1)..x+3]
                let param1 = if modes.[0] = 0 then instructionSet.[t.[0]] else t.[0]
                let param2 = if modes.[1] = 0 then instructionSet.[t.[1]] else t.[1]
                instructionSet.[t.[2]] <- if param1 < param2 then 1 else 0
                executeOnPhase (x+4)

            | 8 -> // EQUALS
                let t :int[] = instructionSet.[(x+1)..x+3]
                let param1 = if modes.[0] = 0 then instructionSet.[t.[0]] else t.[0]
                let param2 = if modes.[1] = 0 then instructionSet.[t.[1]] else t.[1]
                instructionSet.[t.[2]] <- if param1 = param2 then 1 else 0
                executeOnPhase (x+4)

            | 99 -> ()
            | x -> failwithf "Unexpected OP Code %i" x        

        executeOnPhase 0

        output

//Part 1
// let phases = [|0..4|]

// Part 2
let phases = [|5..9|]

let generator :seq<int * int * int * int * int * int> = seq {
    for a in phases do
        for b in phases do
            for c in phases do
                for d in phases do
                    for e in phases do
                        let candidate =
                            [|a;b;c;d;e|]
                            |> Array.groupBy id
                            |> Array.map (fun (_,a) -> a.Length)
                        // Strip off the re-used phases
                        if Array.max candidate = 1 then 
                            let rec feedback start =
                                let opResult = start |> runProgram a |> runProgram b |> runProgram c |> runProgram d |> runProgram e
                                if opResult = start then
                                    opResult
                                else
                                    feedback opResult
                                
                            yield (a,b,c,d,e, feedback 0)
}

generator |> Seq.toArray |> Seq.sortByDescending sxth |> Seq.head

// 0
// |> runProgram 9 |> runProgram 8 |> runProgram 7 |> runProgram 6 |> runProgram 5
// |> runProgram 9 |> runProgram 8 |> runProgram 7 |> runProgram 6 |> runProgram 5
// |> runProgram 9 |> runProgram 8 |> runProgram 7 |> runProgram 6 |> runProgram 5
// |> runProgram 9 |> runProgram 8 |> runProgram 7 |> runProgram 6 |> runProgram 5
// |> runProgram 9 |> runProgram 8 |> runProgram 7 |> runProgram 6 |> runProgram 5
// |> runProgram 9 |> runProgram 8 |> runProgram 7 |> runProgram 6 |> runProgram 5
// |> runProgram 9 |> runProgram 8 |> runProgram 7 |> runProgram 6 |> runProgram 5
// |> runProgram 9 |> runProgram 8 |> runProgram 7 |> runProgram 6 |> runProgram 5