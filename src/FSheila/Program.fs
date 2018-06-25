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
    let testProc = parse testGrammar.procRule "proc teste(x,y) {
                                                    var x = 4;
                                                    x := x + 1;
                                                }"
    let testModule = parse testGrammar.moduleRule "module Fact-Rec var y proc fact(x) { if ~(x == 0) {
		                                                 y := y * x ;
		                                                 x := 1;
                                                       }
                                                       }
                                                       end"//problema tá depois do if, no meio da condbool.
    let testModule2 = parse testGrammar.moduleRule "module Fact-Rec
                                                    var y
                                                    init y = 4
                                                    proc fact(x) {
                                                    var y = 5, x = 4;
                                                      while ~(x == 0) {
                                                          y := y * x; 
                                                          x := x - 1; 
                                                        } 
                                                    }
                                                    end"

    //eSMC.fillEnviroment
    getFromParser testModule2 eSMC
    //eSMC.aKindOfMagic
    //eSMC.print

    
    let sheila = Console.ReadLine()
    printfn "%A" sheila
    0

    