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
                                            dj := dj * dj;;
                                            if (dj == 16) {
                                              sheila := true;
                                            } else {
                                              sheila := 4;
                                            };"
    //let test = parse testGrammar.generalRule "var x = 1, a = 2; a := 3; xvideos := 4;"


    //printfn "%A" teste8
    //let x r =
    //    match r with 
    //    | Success k -> printfn "Input = %A" k.value;
    //    | Failure x -> printfn "%A" x.index
    //x gordo
    //eSMC.fillEnviroment
    getFromParser dj eSMC
    eSMC.aKindOfMagic
    eSMC.print

    
    let sheila = Console.ReadLine()
    printfn "%A" sheila
    0

    