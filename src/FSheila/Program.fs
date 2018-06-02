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
                                           x := x - 1
                                           }"
    let fat2 = parse testGrammar.generalRule "while ~(x == 0) {
                                                    y := y + 1;
                                                    x := x - 1;
                                                    sheila := false}"
    let teste = parse testGrammar.realSeqVarRule "var x = sheila * xsheila"
    let teste2 = parse testGrammar.realSeqConstRule "const x = aaaaaa" //, y , sjsj, lee"
    let teste3 = parse testGrammar.realSeqVarRule "var x = 3 , xsxs = kkkk + 4, aaaaaa = 0, bbbbb = false, aa = x * 4"
    //let teste4 = parse testGrammar.realSeqConstRule "const x = burger , xsxs = sheila / 4 , eeee = xhub"
    //let teste5 = parse testGrammar.seqDecRule "const x = 4 , xsxs = sheila / 4 , eeee = true ;
    //                                           var x = videos"
    let teste6 = parse testGrammar.XBlockRule "{ y := y * x;
                                                x := x - 1;
                                                sheila := y > x and x > 0 }"

    let teste7 = parse testGrammar.realSeqVarRule "var x = videos, y = sheila, zu = 4*gh"
    let teste8 = parse testGrammar.decRule "var x = videos, y = sheila, zu = 4*gh;
                                            const asdasd = 34"  //esse caso não é permitido por construção. Se um bloco é criado 
                                                                //(por alguma declaração, necessariamente ele deve ter um ou mais comandos seguidos (ou uma declaração)
    let teste8 = parse testGrammar.generalRule "var x = videos, y = sheila, zu = 4*gh;
                                            const asdasd = 34;
                                            xvideos := 4;
                                            xsheila := xvideos * xvideos
                                            while ~(x == 0) {
                                                    y := y + 1;
                                                    x := x - 1;
                                                    sheila := false}
                                            "
    let testeErickQuebrado = parse testGrammar.decRule "var erick= 1; const a = 2; erick := a + erick"
    //printfn "%A" teste8
    //let x r =
    //    match r with 
    //    | Success k -> printfn "Input = %A" k.value;
    //    | Failure x -> printfn "%A" x.index
    //x teste8
    //eSMC.fillEnviroment
    getFromParser testeErickQuebrado eSMC
    eSMC.aKindOfMagic
    eSMC.print

    
    let sheila = Console.ReadLine()
    printfn "%A" sheila
    0