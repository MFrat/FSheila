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
                                        

    let gordo = parse testGrammar.generalRule "var y = 5, x = 4;
                                           while ~(x == 0) {
                                             y := y * x; 
                                             x := x - 1;
                                           }"

    let dj = parse testGrammar.generalRule "var dj = 4, sheila = false, asd = 1;
                                            dj := dj * dj;
                                            if (dj == 16) {
                                              sheila := true;
                                            } else {
                                              asd := 4;
                                            }"
    
    //let vaiDarRuim = parse testGrammar.generalRule "const a = 1; a := 2;"

    let testProc = parse testGrammar.procRule "proc fact(x) {
                                                       var y = 5, x = 4;
                                                       while ~(x == 0) {
                                                         y := y * x; 
                                                         x := x - 1; 
                                                       } 
                                                   }"

    //eSMC.fillEnviroment
    getFromParser testProc eSMC
    //eSMC.aKindOfMagic
    //eSMC.print

    
    let sheila = Console.ReadLine()
    printfn "%A" sheila
    0

    