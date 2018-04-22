// Saiba mais sobre F# em http://fsharp.org
// Veja o projeto 'F# Tutorial' para obter mais ajuda.
module FSheila.FSheila

open ScanRat
open System

open FSheila
open Parser
open Smc
      
[<EntryPoint>]
let main argv = 
    //nota: tornar possível 3 + 4 <= 5 - 8, ou seja, operações matemáticas dentro de booleanas. Se isso for necessário basta trocar number por EXP (ou explicitamente por cada uma das comparações numéricas.
    let testGrammar = new PEGParser()
    let teste = parse testGrammar.calcOp "-21 + 5555 + 3 + 4 * 666 /   5"
    let teste = parse testGrammar.boolOp "true and 3==3 and true and false";
    let teste = parse testGrammar.boolOp "~(true and ~false)"
    //let teste = parse testGrammar.boolOp "~(true and ~(3<>4))"
    //let teste2 = parse testGrammar.boolOp "true and false or false and true"
    //let teste = parse testGrammar.boolOp "3<=4 and 4<>5 or true and false and 666  <= 4 or 1981> 2007"
    //let teste = parse testGrammar.varRule "var x , y , z, a"
    //let teste = parse testGrammar.constRule "const abc , x , y , a69"
    let teste3 = parse testGrammar.initRule "init x = 2 "
    //let teste = parse testGrammar.assignRule "a := 444*58- 69 ; u := 1+2*333 + 8"
    //let teste = parse testGrammar.boolOp "~(true and ~(3<>4 and false))" 
    //embora o teste abaixo para mim nao deveria funcionar, boa sorte se quiser fazer a sequencia funcionar de boa de outra forma
    //let teste = parse testGrammar.seqRule "a:= 333"
    //let teste = parse testGrammar.seqRule "a := 333 ; b := 444 ; ccc := 69-66*8 ; cas := 0 "//; b:= 555"
    //let teste = parse testGrammar.seqRule " a:= 333;b := 888 * 7 ; c := 666*777/8"
    //let teste = parse testGrammar.loopRule "while 3<>4 { 
    //                                              sheila3 := 555+8 ; 
    //                                              sheila := 9999 ; ati := 999*555 ;
    //                                              sheila2 := 4 <> 4 and false ;
    //                                              sheila3 := 4==6 or ~(3<5 and false);
    //
    //                                              aaa := 1+9-5*8  
    //                                              }"
    //let teste2 = parse testGrammar.ifRule "if 4<>4 tim := 4 *2 +5 else maia := 4"
    //let teste2 = parse testGrammar.ifRule "if 7 <= 9 { 
    //                                          sheila := 3;
    //                                          sheila := 4 + 5 } else sheila := 4"
    //let teste2 = parse testGrammar.ifRule "if 7 <= 9 sheila := 5/2 + 4 else { 
    //                                                   sheila := 4;
    //                                                    spacer := 69;
    //                                                   star := 4*78/9-6}"
    let teste2 = parse testGrammar.ifRule "if 7 <= 9 {
                                           sheila := 444;
                                           stack := 685/5;
                                           overflow := 48 + abcd * sheila
                                           } else    { 
                                                       sheila := 4;
                                                       spacer := 69;
                                                       star := 4*78/9-6}"
    //let teste2 = parse testGrammar.seqRule "sheila := 24*999 + 3"



    //let teste = parse testGrammar.assignRule "xda := true or 4<>5 and sheila4 <= 4  and ~(sheila2 and sheila)"
    let teste = parse testGrammar.calcOp "-21 + 5555 + 3 + 4 * 666 /   5"
    //let smc = new SMC()
    getFromParser teste
    printfn "X = %A" (X)
    printfn "S = %A" (S)
    printfn "C = %A" (C)
    calculator C
    printfn "X = %A" (X)
    printfn "S = %A" (S)
    printfn "C = %A" (C)
    //TESTES de SMC daqui para baixo:
    //let x = new SMC()
    let get exp = 
      match exp with
        | Success s ->  s.value
        | Failure f -> failwith "fail"
    //let ae = valueOf teste
    //printfn "%A" (get teste)
    let sheila = Console.ReadLine()
    printfn "%A" sheila
    //printfn "%A" argv
    0 // retornar um código de saída inteiro