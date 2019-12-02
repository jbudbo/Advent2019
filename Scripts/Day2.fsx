#if INTERACTIVE
#load "Utils.csx"
#endif

open System
open System.IO

let readData (path:string) =  File.ReadAllText(path) 

let part1 (dataFile:string) =
    0 
    