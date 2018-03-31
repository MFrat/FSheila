// Saiba mais sobre F# em http://fsharp.org
// Veja o projeto 'F# Tutorial' para obter mais ajuda.
module teste.teste

open ScanRat
open System


//quem conseguir fazer isso funcionar ganha um premio. ele diz que uma gramática é umac oleção de regras mas não fala como que essa coleção tem que ser estruturada.
type PEGParser () = 
    member this.number = oneOf "0123456789" --> fun a -> int(a) - int('0')
    member this.negNumber = ~~"-" + this.number --> fun a -> int (-snd(a))- int('0')
    member this.addRule =  (this.number |- this.negNumber) + ~~"+" + (this.number |- this.negNumber) --> fun (a,b) -> fst(a) + b
    member this.subRule = (this.number |- this.negNumber) + ~~"+" + (this.number |- this.negNumber) --> fun (a,b) -> fst(a) - b
    member this.divRule = (this.number |- this.negNumber) + ~~"/" + (this.number |- this.negNumber) --> fun (a,b) -> fst(a) / b
    member this.mulRule = (this.number |- this.negNumber) + ~~"+" + (this.number |- this.negNumber) --> fun (a,b) -> fst(a) * b

    //let PEGParser =
     //   let number = oneOf "0123456789" --> fun a -> int(a) - int('0')
     //   let negNumber = ~~"-" + this.number --> fun a -> int (-snd(a))- int('0')
     //   let addRule =  (this.number |- this.negNumber) + ~~"+" + (this.number |- this.negNumber) --> fun (a,b) -> fst(a) + b
     //   let subRule = (this.number |- this.negNumber) + ~~"+" + (this.number |- this.negNumber) --> fun (a,b) -> fst(a) - b
     //   let divRule = (this.number |- this.negNumber) + ~~"/" + (this.number |- this.negNumber) --> fun (a,b) -> fst(a) / b
     //   let mulRule = (this.number |- this.negNumber) + ~~"+" + (this.number |- this.negNumber) --> fun (a,b) -> fst(a) * b

//[<EntryPoint>]
let main argv = 
    let sheila = Console.ReadLine()
    printfn "%A" sheila
    //printfn "%A" argv
    0 // retornar um código de saída inteiro
