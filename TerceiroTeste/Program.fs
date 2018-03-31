// Saiba mais sobre F# em http://fsharp.org
// Veja o projeto 'F# Tutorial' para obter mais ajuda.
module teste.teste

open ScanRat
open ScanRatCombinators
open System



type PEGParser () = 
        //vale a pena lembrar que os operadores --> vão sair; a semântica das operãções vão vir da BPLC
        
        member this.number = oneOf "0123456789" --> fun a -> int(a) - int('0')
        member this.negNumber = ~~"-" + this.number --> fun a -> -snd(a)
       
        //math ruless
        member this.addRule = (this.number |- this.negNumber) + ~~"+" + (this.number |- this.negNumber) --> fun (a,b) -> fst(a) + b
        member this.subRule = (this.number |- this.negNumber) + ~~"-" + (this.number |- this.negNumber) --> fun (a,b) -> fst(a) - b
        member this.divRule = (this.number |- this.negNumber) + ~~"/" + (this.number |- this.negNumber) --> fun (a,b) -> fst(a) / b
        member this.mulRule = (this.number |- this.negNumber) + ~~"*" + (this.number |- this.negNumber) --> fun (a,b) -> fst(a) * b

        //boolean expressions:
        member this.eqRule = (this.number |- this.negNumber) + ~~"==" + (this.number |- this.negNumber) --> fun (a,b) -> fst(a) = b
        member this.negRule = ~~"~" + (this.number |- this.negNumber)
        member this.lebRule = (this.number |- this.negNumber) + ~~"<" + (this.number |- this.negNumber)
        member this.leqRule = (this.number |- this.negNumber) + ~~"<=" + (this.number |- this.negNumber)
        member this.gebRule = (this.number |- this.negNumber) + ~~">" + (this.number |- this.negNumber)
        member this.geqRule = (this.number |- this.negNumber) + ~~">=" + (this.number |- this.negNumber)
        member this.orRule = (this.number |- this.negNumber) + ~~"or" + (this.number |- this.negNumber)
        member this.andRule = (this.number |- this.negNumber) + ~~"and" + (this.number |- this.negNumber)
       

       //commands:
       //member this.assignRule =   não sei definir uma regra que parseie 1 letra seguida de 0 ou mais letras e números pq não consigo achar uma função que puxe várias coisas ao mesmo tempo.
   

[<EntryPoint>]
let main argv = 
    //if 3 = 3 then printfn "a"
    let testGrammar = new PEGParser()
    let teste = parse testGrammar.negRule "~3"
    printfn "%A" teste
    let sheila = Console.ReadLine()
    printfn "%A" sheila
    //printfn "%A" argv
    0 // retornar um código de saída inteiro
