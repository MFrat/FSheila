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
                                        

    let gordo = parse testGrammar.generalRule "var y = 10, x = 10;
                                           while ~(x == 0) {
                                             y := y * x; 
                                             x := x - 1;
                                           }"

    let dj = parse testGrammar.generalRule "var dj = 4, sheila = false;
                                            dj := dj * dj;
                                            if (dj == 16) {
                                              sheila := true;
                                            } else {
                                              sheila := 4;
                                            }"
    
    //eSMC.fillEnviroment
    getFromParser gordo eSMC
    eSMC.aKindOfMagic
    eSMC.print

    
    let sheila = Console.ReadLine()
    printfn "%A" sheila
    0

    