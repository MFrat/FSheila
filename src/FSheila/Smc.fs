module FSheila.Smc

open ScanRat
open FSheila
open Parser
open System.Collections.Generic

let X = new Stack<string>()
let S = new Stack<Cmd>()
//let M = new Map<string, Cmd>()
let C = new Stack<Cmd>()

//Faltando neg
let calculatorBool (X: Stack<string>) (S: Stack<Cmd>) :bool = 
    while X.Count <> 0 do
        let op = X.Pop()
        let d1 = S.Pop()
        let d2 = S.Pop()
        match (d1,d2) with
        | Boolean d1, Boolean d2 ->
            match op with
            | "And" -> S.Push(Boolean(d1 && d2))
            | "Or" -> S.Push(Boolean(d1 || d2))
    match S.Pop() with
    | Boolean a -> a


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
 
 //Faltando <>
let rec stackator (exp) =
    match exp with
    | Add (a, b) -> X.Push("Add"); match (a,b) with
                                    | Number x, Number y -> S.Push(Number x); S.Push(Number y)
                                    | Number x , d -> S.Push(Number x); stackator d; 
                                    | d , Number y -> S.Push (Number y); stackator d
                                    | v , k -> stackator (v); stackator (k)
    | Subtract (a, b) -> X.Push("Subtract"); match (a,b) with
                                    | Number x, Number y -> S.Push(Number x); S.Push(Number y)
                                    | Number x , d -> S.Push(Number x); stackator d; 
                                    | d , Number y ->  S.Push (Number y); stackator d
                                    | v , k -> stackator (v); stackator (k)// stackator a; stackator b
    | Multiply (a, b) -> X.Push("Multiply"); match (a,b) with
                                    | Number x, Number y -> S.Push(Number x); S.Push(Number y)
                                    | Number x , d -> S.Push(Number x); stackator d; 
                                    | d , Number y ->  S.Push (Number y); stackator d
                                    | v , k -> stackator (v); stackator (k) //stackator a; stackator b
    | Divide (a, b) -> X.Push("Divide"); match (a,b) with
                                    | Number x, Number y -> S.Push(Number x); S.Push(Number y)
                                    | Number x , d -> S.Push(Number x); stackator d; 
                                    | d , Number y ->  S.Push (Number y); stackator d
                                    | v , k -> stackator (v); stackator (k) //stackator a; stackator b
    | And (a, b) -> X.Push("And"); match a,b with
                                    | Boolean x, Boolean y -> S.Push(Boolean x); S.Push(Boolean y)
                                    | Boolean x , d -> S.Push(Boolean x); stackator d; 
                                    | d , Boolean y -> S.Push (Boolean y); stackator d
                                    | v , k -> stackator (v); stackator (k)
    | Or (a, b) -> X.Push("Or");  match a,b with
                                    | Boolean x, Boolean y -> S.Push(Boolean x); S.Push(Boolean y)
                                    | Boolean x , d -> S.Push(Boolean x); stackator d; 
                                    | d , Boolean y -> S.Push (Boolean y); stackator d
                                    | v , k -> stackator (v); stackator (k)
    | Neg a -> X.Push("Neg"); match a with 
                              | Boolean x -> S.Push(Boolean x)
                              | d -> stackator(d)

let getFromParser (exp) =
    match exp with
    | Success r -> printfn "Input = %A" (r.value); stackator r.value
    | Failure _ -> failwith "Parsing falhou!"