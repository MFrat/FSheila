// Saiba mais sobre F# em http://fsharp.org
// Veja o projeto 'F# Tutorial' para obter mais ajuda.
module teste.teste

open ScanRat
open ScanRatCombinators
open System



type PEGParser () = 
        //vale a pena lembrar que os operadores --> vão sair; a semântica das operãções vão vir da BPLC

        //basic shit:  
        member this.posNumber = oneOf "0123456789" --> fun a -> int(a) - int('0')
        member this.negNumber = ~~"-" + this.posNumber --> fun a -> -snd(a)
        member this.lLetter = oneOf "abcdefghijklmnopqrstuvwxyz" --> fun a -> str(a)
        member this.uLetter = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ" --> fun a -> str(a)
        member this.number = this.posNumber |- this.negNumber
        member this.letter = this.lLetter |- this.uLetter
        member this.identifier = this.lLetter |- (this.lLetter + this.letter)
       
        //math expressions:
        member this.addRule = this.number + ~~"+" + this.number --> fun (a,b) -> fst(a) + b
        member this.subRule = this.number + ~~"-" + this.number --> fun (a,b) -> fst(a) - b
        member this.divRule = this.number + ~~"/" + this.number --> fun (a,b) -> fst(a) / b
        member this.mulRule = this.number + ~~"*" + this.number --> fun (a,b) -> fst(a) * b
        member this.mathRule = (this.addRule |- this.subRule) |- (this.mulRule |- this.divRule)

        //boolean expressions:
        member this.eqRule = this.number + ~~"==" + this.number --> fun (a,b) -> fst(a) = b
        member this.negRule = ~~"~" + this.number
        member this.lebRule = this.number + ~~"<" + this.number
        member this.leqRule = this.number + ~~"<=" + this.number
        member this.gebRule = this.number + ~~">" + this.number
        member this.geqRule = this.number + ~~">=" + this.number
        member this.orRule = this.number + ~~"or" + this.number
        member this.andRule = this.number + ~~"and" + this.number
        member this.boolRule = (this.eqRule |- this.negRule) |- (this.lebRule |- this.leqRule) |- (this.gebRule |- this.geqRule) |- (this.andRule |- this.orRule)

        //general expressions:
        member this.expRule = (this.mathRule |- this.boolRule)   
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
