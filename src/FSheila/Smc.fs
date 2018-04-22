// Saiba mais sobre F# em http://fsharp.org
// Veja o projeto 'F# Tutorial' para obter mais ajuda.
module FSheila.Smc

open ScanRat
open FSheila
open Parser
open System.Collections.Generic //daqui vem o dicionario

let X = new Stack<string>()
let S = new Stack<Cmd>()
//let M = new Map<string, Cmd>()
let C = new Stack<Cmd>()
    
let getFromParser (exp) =
    match exp with
    | Success r -> C.Push(r.value)
    | Failure _ -> failwith "Parsing falhou!"
    //de acordo com as transições em SMC (descritas nas páginas 15, 16 e 17

let rec calculator (C: Stack<Cmd>) = //a ideia parece ser exatamente isso
    while C.Count <> 0 do
        let k = C.Pop()
        printfn "k = %A" k
        match k with
        | Add (a, b) -> X.Push("Add")// (calculator a) + (calculator b) //|> push Add S
        | Subtract (a, b) -> X.Push("Subtract")//(calculator a) - (calculator b) //|> push Subtract S
        | Multiply (a, b) -> X.Push("Multiply")//(calculator a) * (calculator b) //|> push Multiply S
        | Divide (a, b) -> X.Push("Divide")//(calculator a) / (calculator b) //|> push Divide S
        | And (a, b) -> X.Push("And")//(calculator a) && (calculator b) //|> push And S
        | Or (a, b) -> X.Push("Or")//(calculator a) || (calculator b) //|> push Or S
        | Neg a -> X.Push("Neg")//not(calculator a) //|> push Neg S
        | Number a ->  S.Push(Number a)
        | Boolean a -> S.Push(Boolean a)
