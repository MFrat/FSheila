module FSheila.Smc

open ScanRat
open FSheila
open Parser
open System.Collections.Generic //daqui vem o dicionario

let X = new Stack<string>()
let S = new Stack<Cmd>()
//let M = new Map<string, Cmd>()
let C = new Stack<Cmd>()


let calculator (X: Stack<string>) (S: Stack<Cmd>) :int = 
    while X.Count <> 0 do
        let op = X.Pop()
        let d1 = S.Pop()
        let d2 = S.Pop()
        match (d1,d2) with
        | Number d1, Number d2 ->
            match op with
            | "Add" -> S.Push(Number(d1 + d2))
            | "Subtract" -> S.Push(Number(d1 - d2))
            | "Multiply" -> S.Push(Number(d1 * d2))
            | "Divide" -> S.Push(Number(int(d1 / d2)))
    match S.Pop() with
    | Number a ->
        a
 
let rec stackator (exp) =
    match exp with
    | Add (a, b) -> X.Push("Add"); stackator a; stackator b
    | Subtract (a, b) -> X.Push("Subtract"); stackator a; stackator b
    | Multiply (a, b) -> X.Push("Multiply"); stackator a; stackator b
    | Divide (a, b) -> X.Push("Divide"); stackator a; stackator b
    | And (a, b) -> X.Push("And"); stackator a; stackator b
    | Or (a, b) -> X.Push("Or"); stackator a; stackator b
    | Neg a -> X.Push("Neg"); stackator a
    | Number a ->  S.Push(Number a)
    | Boolean a -> S.Push(Boolean a)

let getFromParser (exp) =
    match exp with
    | Success r -> printfn "Input = %A" (r.value); stackator r.value
    | Failure _ -> failwith "Parsing falhou!"