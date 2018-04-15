// Saiba mais sobre F# em http://fsharp.org
// Veja o projeto 'F# Tutorial' para obter mais ajuda.
module FSheila.Smc

open FSheila
open Parser
open ScanRat

type Stack = 
    | StackContents of _ list

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
    | Add (a, b) -> push Add S, (calculator a), (calculator b)
    | Subtract (a, b) -> push Subtract S |> (calculator a) - (calculator b)
    | Multiply (a, b) -> push Multiply S |> (calculator a) * (calculator b)
    | Divide (a, b) -> push Divide S |> (calculator a) / (calculator b)
    | Number a -> push a C |>  a

let math exp = 
    match exp with
    | Success s -> calculator s.value
    | Failure f -> failwith "deu bosta então se foda"   