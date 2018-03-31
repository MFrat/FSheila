// Saiba mais sobre F# em http://fsharp.org
// Veja o projeto 'F# Tutorial' para obter mais ajuda.
module teste.teste

open ScanRat
open System


//quem conseguir fazer isso funcionar ganha um premio. ele diz que uma gramática é umac oleção de regras mas não fala como que essa coleção tem que ser estruturada.

type PEGParser () = 
        //math rules
        member this.number = oneOf "0123456789" --> fun a -> int(a) - int('0')
        member this.negNumber = ~~"-" + this.number --> fun a -> int (-snd(a))- int('0')
        //member this.arithop = oneOf "+-*/|"
        //member this.exp = this.id
        member this.addRule =  (this.number |- this.negNumber) + ~~"+" + (this.number |- this.negNumber) --> fun (a,b) -> fst(a) + b
        member this.subRule = (this.number |- this.negNumber) + ~~"-" + (this.number |- this.negNumber) --> fun (a,b) -> fst(a) - b
        member this.divRule = (this.number |- this.negNumber) + ~~"/" + (this.number |- this.negNumber) --> fun (a,b) -> fst(a) / b
        member this.mulRule = (this.number |- this.negNumber) + ~~"*" + (this.number |- this.negNumber) --> fun (a,b) -> fst(a) * b

        //boolean expressions: me perdi
        //member this.eqRule = (this.number |- this.negNumber) + ~~"==" + (this.number |- this.negNumber) --> fun (a,b) -> fst(a) = b
       

       //commands:
        //member this.assignRule =  + 
   

[<EntryPoint>]
let main argv = 
    //if 3 = 3 then printfn "a"
    let testGrammar = new PEGParser()
    let teste = parse testGrammar.eqRule "3==2"
    printfn "%A" teste
    let sheila = Console.ReadLine()
    printfn "%A" sheila
    //printfn "%A" argv
    0 // retornar um código de saída inteiro
