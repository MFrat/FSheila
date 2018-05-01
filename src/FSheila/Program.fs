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
    //nota: tornar possível 3 + 4 <= 5 - 8, ou seja, operações matemáticas dentro de booleanas. Se isso for necessário basta trocar number por EXP (ou explicitamente por cada uma das comparações numéricas.
    let testGrammar = new PEGParser()
    let fat = parse testGrammar.generalRule "while ~(x == 0) {
                                           y := y * x;
                                           x := x - 1;
                                           sheila := y > x and x > 0
                                           }"
    getFromParser fat
    aKindOfMagic S M C
    printSMC S M C
    
    let sheila = Console.ReadLine()
    printfn "%A" sheila
    0