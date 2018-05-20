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
    
    let fat = parse testGrammar.generalRule "while ~(x == 0) {
                                           y := y * x;
                                           x := x - 1;
                                           sheila := y > x and x > 0
                                           }"
    let fat2 = parse testGrammar.generalRule "while ~(x == 0) {
                                                    y := y + 1;
                                                    x := x - 1;
                                                    sheila := false}"
    let teste = parse testGrammar.realSeqVarRule "var x = sheila * xsheila"
    let teste2 = parse testGrammar.realSeqConstRule "const x = aaaaaa" //, y , sjsj, lee"
    let teste3 = parse testGrammar.realSeqVarRule "var x = 3 , xsxs = kkkk + 4, aaaaaa = 0, bbbbb = false, aa = x * 4"
    let teste4 = parse testGrammar.realSeqConstRule "const x = burger , xsxs = sheila / 4 , eeee = xhub"
    //let teste5 = parse testGrammar.seqDecRule "const x = 4 , xsxs = sheila / 4 , eeee = true ;
    //                                           var x = videos"
    
    //printfn "%A" teste4
    let x r =
        match r with 
        | Success k -> printfn "Input = %A" k.value;
        | Failure x -> printfn "sheila"
    x teste4
    //eSMC.fillMemory
    //getFromParser fat eSMC
    //eSMC.aKindOfMagic
    //eSMC.print

    
    let sheila = Console.ReadLine()
    printfn "%A" sheila
    0