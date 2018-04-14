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
    let valueOf r =
        match r with
        | Success s -> s.value
        | Failure _ -> failwith "parse failed"
    //nota: tornar possível 3 + 4 <= 5 - 8, ou seja, operações matemáticas dentro de booleanas
    let testGrammar = new PEGParser()
    //let teste = parse testGrammar.calcOp "-21 + 5555 + 3 + 4 * 666 /   5"
    //let teste = parse testGrammar.boolOp "true and 3==3 and true and false";
    //let teste = parse testGrammar.boolOp "~(true and ~false)"
    //let teste = parse testGrammar.boolOp "~(true and ~(3<>4))"
    //let teste2 = parse testGrammar.boolOp "true and false or false and true"
    //let teste = parse testGrammar.boolOp "3<=4 and 4<>5 or true and false and 666  <= 4 or 1981> 2007"
    //let teste = parse testGrammar.varRule "var x , y , z, a"
    //let teste = parse testGrammar.constRule "const abc , x , y , a69"
    //let teste = parse testGrammar.initRule "init x = 2 , y = 555*6/8 , abhe =   1981"
    //let teste = parse testGrammar.assignRule "a := 444*58- 69 ; u := 1+2*333 + 8"
    //let teste = parse testGrammar.boolOp "~(true and ~(3<>4 and false))" 
    //let teste = parse testGrammar.blockRule "{ sexo := 3}"
    //embora o teste abaixo para mim nao deveria funcionar, boa sorte se quiser fazer a sequencia funcionar de boa de outra forma
    //let teste = parse testGrammar.seqRule "a:= 333"
    //let teste = parse testGrammar.seqRule "a := 333 ; b := 444 ; ccc := 69-66*8 ; cas := 0 "//; b:= 555"
    //let teste = parse testGrammar.seqRule " a:= 333;b := 888 * 7 ; c := 666*777/8"
    let teste = parse testGrammar.loopRule "while 3<>4 { 
                                                  sheila3 := 555+8 ; 
                                                  sheila := 9999 ; ati := 999*555 ;


                                                  aaa := 1+9-5*8  
                                                  }"
    //let teste2 = parse testGrammar.ifRule "if true a := 3 else a:=4"
    //let ae = valueOf teste
    printfn "%A" teste
    //printfn "%A" teste
    let sheila = Console.ReadLine()
    printfn "%A" sheila
    //printfn "%A" argv
    0 // retornar um código de saída inteiro