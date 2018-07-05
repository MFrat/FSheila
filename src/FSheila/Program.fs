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
                                                       end"

    let testModule2 = parse testGrammar.sheilaRule "module Fact-Rec
                                                    var y
                                                    init y = 4
                                                    fun fact(x) {
                                                    var y = 5, x = 4;
                                                      while ~(x == 0) {
                                                          y := y * x; 
                                                          x := x - 1; 
                                                        }
                                                        return x;
                                                    }
                                                    proc xhub(x) {
                                                    var y = 5, x = 4;
                                                      while ~(x == 0) {
                                                          y := y * x; 
                                                          xhub(x); 
                                                        }
                                                    }
                                                    end  fact(4);"

    let testModule2 = parse testGrammar.sheilaRule "module Fact-Rec
                                                    var y
                                                    init y = 4
                                                    proc fact(x) {
                                                    var y = 1;
                                                      while ~(x == 0) {
                                                          y := y * x; 
                                                          x := x - 1; 
                                                        }
                                                    }
                                                    end  fact(5);"
    let retTest = parse testGrammar.sheilaRule "module asdas
                                                var y
                                                init y = 4
                                                fun videos(x) {
                                                    var y = 3;
                                                    return y;
                                                } 
                                                end videos(4);"
    

    //let testModule2 = parse testGrammar.callRule "fact(4);"

    //eSMC.fillEnviroment
    getFromParser retTest eSMC
    eSMC.aKindOfMagic
    eSMC.print

    let sheila = Console.ReadLine()
    printfn "%A" sheila
    0

    