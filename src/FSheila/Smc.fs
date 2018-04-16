// Saiba mais sobre F# em http://fsharp.org
// Veja o projeto 'F# Tutorial' para obter mais ajuda.
module FSheila.Smc

open ScanRat
open FSheila
open Parser

type Stack = 
    | StackContents of Exp list

let push x (StackContents contents) =   
    StackContents (x::contents)

let pop (StackContents contents) = 
    match contents with 
    | top::rest -> 
        let newStack = StackContents rest
        (top,newStack)
    | [] -> 
        failwith "Stack underflow"

let S = StackContents []
let C = StackContents []

let rec calculator exp = 
    match exp with
    | Add (a, b) -> (calculator a) + (calculator b) //|> push Add S
    | Subtract (a, b) -> (calculator a) - (calculator b) //|> push Subtract S
    | Multiply (a, b) -> (calculator a) * (calculator b) //|> push Multiply S
    | Divide (a, b) -> (calculator a) / (calculator b) //|> push Divide S
    | Number a ->  a //|> push a C

let math exp = 
    match exp with
    | Success s -> calculator s.value
    | Failure f -> failwith "deu bosta então se foda"   