open System.IO

let runProgram input =
    let intToInts =
        let tokenize (i:int) = if i = 0 then None else Some(i%10,i/10)
        let crack (i:int) = i |> Array.unfold tokenize |> Array.rev
        crack

    let padLeft (arr:int[]) :int[] =
        let result = Array.zeroCreate 3
        arr |> Array.rev |> Array.iteri (fun acc a -> result.[acc] <- a)
        result

    File.ReadAllText("inputs/Day5").Split ','
    |> Array.map int
    |> fun instructionSet ->
        let rec execute x =
            let op = instructionSet.[x] % 100
            let modes = instructionSet.[x] / 100 |> intToInts |> padLeft

            match op with
            | 1 -> // ADD
                let t :int[] = instructionSet.[(x+1)..x+3]
                let param1 = if modes.[0] = 0 then instructionSet.[t.[0]] else t.[0]
                let param2 = if modes.[1] = 0 then instructionSet.[t.[1]] else t.[1]
                instructionSet.[t.[2]] <- param1 + param2
                execute (x + 4)

            | 2 -> // MULTIPLY
                let t :int[] = instructionSet.[(x+1)..x+3]
                let param1 = if modes.[0] = 0 then instructionSet.[t.[0]] else t.[0]
                let param2 = if modes.[1] = 0 then instructionSet.[t.[1]] else t.[1]
                instructionSet.[t.[2]] <- param1 * param2
                execute (x+4)

            | 3 -> // INPUT
                let t :int = instructionSet.[x+1]
                instructionSet.[t] <- input
                execute (x+2)

            | 4 -> // OUTPUT
                let t :int = instructionSet.[x+1]
                printfn "Output %i" instructionSet.[t]
                execute (x+2)

            | 5 -> // JUMP IF TRUE
                let t :int[] = instructionSet.[(x+1)..x+3]
                let param1 = if modes.[0] = 0 then instructionSet.[t.[0]] else t.[0]
                let param2 = if modes.[1] = 0 then instructionSet.[t.[1]] else t.[1]
                if param1 <> 0 then
                    execute (param2)
                else
                    execute (x + 3) 

            | 6 -> // JUMP IF FALSE
                let t :int[] = instructionSet.[(x+1)..x+3]
                let param1 = if modes.[0] = 0 then instructionSet.[t.[0]] else t.[0]
                let param2 = if modes.[1] = 0 then instructionSet.[t.[1]] else t.[1]
                if param1 = 0 then 
                    execute (param2)
                else
                    execute (x + 3) 

            | 7 -> // LESS THAN
                let t :int[] = instructionSet.[(x+1)..x+3]
                let param1 = if modes.[0] = 0 then instructionSet.[t.[0]] else t.[0]
                let param2 = if modes.[1] = 0 then instructionSet.[t.[1]] else t.[1]
                instructionSet.[t.[2]] <- if param1 < param2 then 1 else 0
                execute (x+4)

            | 8 -> // EQUALS
                let t :int[] = instructionSet.[(x+1)..x+3]
                let param1 = if modes.[0] = 0 then instructionSet.[t.[0]] else t.[0]
                let param2 = if modes.[1] = 0 then instructionSet.[t.[1]] else t.[1]
                instructionSet.[t.[2]] <- if param1 = param2 then 1 else 0
                execute (x+4)

            | 99 -> ()
            | x -> failwithf "Unexpected OP Code %i" x

        execute 0

runProgram 5