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
    let grammar = new PEGParser()
    let eSMC = new ESMC()

    let fact_proc = parse grammar.sheilaRule "module Fact-Rec
                                                    var y
                                                    init y = true
                                                    proc fact(x) {
                                                    var y = 1;
                                                      while ~(x == 0) {
                                                          y := y * x; 
                                                          x := x - 1; 
                                                        } 
                                                    }
                                                    proc main() {
                                                    var k = 3;
                                                    fact(k);
                                                    }
                                                    end  main();"
    let ret_test = parse grammar.sheilaRule "module asdas
                                                var y
                                                init y = 4
                                                fun videos(x) {
                                                    return x;
                                                } end videos(3);"
                                            
    let fact_fun = parse grammar.sheilaRule "module fact-rec
                                                var y
                                                init y = true
                                                fun fact(x) {
                                                  var y = 1;
                                                  while ~(x==0) {
                                                  y := y * x;
                                                  x := x - 1;
                                                  }
                                                     return y;
                                                }
                                                proc main() {
                                                  var k = 3;
                                                  fact(k);
                                                  }
                                                end main();"
                                               
    getFromParser fact_proc eSMC
    eSMC.aKindOfMagic
    eSMC.print

    let sheila = Console.ReadLine()
    printfn "%A" sheila
    0

    