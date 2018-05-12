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
    let testGrammar = new PEGParser()
    let fat = parse testGrammar.generalRule "while ~(x == 0) {
                                           y := y * x;
                                           x := x - 1;
                                           sheila := y > x and x > 0
                                           }"
    let fat2 = parse testGrammar.generalRule "while ~(x == 0) {
                                                    y := y + 1;
                                                    x := x - 1;
                                                    sheila := false}"
    let teste = parse testGrammar.varRule "var x , y , sheila, xsheila"
    let teste2 = parse testGrammar.constRule "const x, y , sjsj, lee"
    //printfn "%A" teste2
    getFromParser fat
    aKindOfMagic S M C
    printSMC S M C
    
    let sheila = Console.ReadLine()
    printfn "%A" sheila
    0