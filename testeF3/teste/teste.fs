module teste
open System
open ScanRat

[<EntryPoint>]
let main argv =
    let buceta =  System.Console.Read();
    printfn "buceta %d" buceta
    0 // return an integer exit code
