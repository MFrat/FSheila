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

let rec calculator (exp) (C: Stack<Cmd>) (S: Stack<Cmd>) = //a ideia parece ser exatamente isso
    match exp with
    | Add (a, b) -> X.Push("Add"); calculator a C S; calculator b C S// (calculator a) + (calculator b) //|> push Add S
    | Subtract (a, b) -> X.Push("Subtract"); calculator a C S; calculator b C S//(calculator a) - (calculator b) //|> push Subtract S
    | Multiply (a, b) -> X.Push("Multiply"); calculator a C S; calculator b C S//(calculator a) * (calculator b) //|> push Multiply S
    | Divide (a, b) -> X.Push("Divide"); calculator a C S; calculator b C S//(calculator a) / (calculator b) //|> push Divide S
    | And (a, b) -> X.Push("And"); calculator a C S; calculator b C S//(calculator a) && (calculator b) //|> push And S
    | Or (a, b) -> X.Push("Or"); calculator a C S; calculator b C S//(calculator a) || (calculator b) //|> push Or S
    | Neg a -> X.Push("Neg"); calculator a C S//not(calculator a) //|> push Neg S
    | Number a ->  S.Push(Number a)
    | Boolean a -> S.Push(Boolean a)

let getFromParser (exp) =
    match exp with
    | Success r -> calculator r.value S C
    | Failure _ -> failwith "Parsing falhou!"
    //de acordo com as transições em SMC (descritas nas páginas 15, 16 e 17