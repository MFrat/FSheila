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
    //NOTA: por motivos técnicos, var seguido de const não funciona!

    //let teste8 = parse testGrammar.ifRule "if (x > 0) { 
    //                                      const alhao = 3, buceta = 4 * caral;
    //                                      var porb = cao; 
    //                                      xvideos := 4 + 4; 
    //                                      gretchen := true ; grete := false;
    //                                       } else { sheila := 4;
    //                                                gretchen := true; }"
                                                     
    //let teste8 = parse testGrammar.loopRule "while ( xvideos <> 0 ) { 
    //                                      const cara = 3, bta = 4 * cao;
    //                                      var hub = carhao, asdasda = xml / cara; 
    //                                      xvideos := 4 + 4; 
    //                                      gretchen := true; grete := false; };"
                                        
    let teste8 = parse testGrammar.XBlockRule "{const cara = 3, bta = 4 * cao;
                                          var hub = carhao, asdasda = xml / cara; 
                                          xvideos := 4 + 4; 
                                          gretchen := true; grete := false; }"
      
                  
    printfn "%A" teste8
    let x r =
        match r with 
        | Success k -> printfn "Input = %A" k.value;
        | Failure x -> printfn "%A" x.index
    x teste8
    //eSMC.fillEnviroment
    //getFromParser testeErickNaoQuebrado eSMC
    //eSMC.aKindOfMagic
    //eSMC.print

    
    let sheila = Console.ReadLine()
    printfn "%A" sheila
    0

    