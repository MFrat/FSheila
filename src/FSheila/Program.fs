module FSheila.FSheila

open ScanRat
open System

open FSheila
open Utils
open Parser
open Smc
      
[<EntryPoint>]
let main argv = 
    printIntro
    M.Add("y", Number(1))
    M.Add("x", Number(5))
    let testGrammar = new PEGParser()
    let fat = parse testGrammar.generalRule "while ~(x == 0) {y := y * x;x := x - 1}"
    getFromParser fat
    aKindOfMagic S M C
    printSMC S M C
    
    let sheila = Console.ReadLine()
    printfn "%A" sheila
    0