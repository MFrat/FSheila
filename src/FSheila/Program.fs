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
    let eSMC = new ESMC()
    eSMC.M.Add("y", Number(bigint 1))
    eSMC.M.Add("x", Number(bigint 10))
    eSMC.M.Add("sheila",Boolean(false))
    let fat = parse testGrammar.generalRule "while ~(x == 0) {
                                           y := y * x;
                                           x := x - 1;
                                           sheila := y > x and x > 0
                                           }"
    let fat2 = parse testGrammar.generalRule "while ~(x == 0) {
                                                    y := y + 1;
                                                    x := x - 1;
                                                    sheila := false}"
    let teste = parse testGrammar.varRule "var x , y , sheila, xsheila, xvideos"
    let teste2 = parse testGrammar.constRule "const x, y , sjsj, lee"
    
    //printfn "%A" teste
    getFromParser fat eSMC
    eSMC.aKindOfMagic
    printSMC eSMC.S eSMC.M eSMC.C
    
    let sheila = Console.ReadLine()
    printfn "%A" sheila
    0