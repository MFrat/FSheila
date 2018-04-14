// Saiba mais sobre F# em http://fsharp.org
// Veja o projeto 'F# Tutorial' para obter mais ajuda.

open System

let sort x =
    x
    |> List.sort

// Returns the new stack
let push item stack = item :: stack

// Returns (item, newStack) tuple or throws if stack is empty
let pop stack =
    match stack with
    | [] -> failwith "Stack is empty"
    | item :: newStack -> item, newStack

let getOps (clause : string) =
    let a = clause.IndexOf("Add")
    let m = clause.IndexOf("Mul")
    let s = clause.IndexOf("Sub")
    let d = clause.IndexOf("Div")
    let ops = sort [a; m; s; d]
    let size = ops.Length
    printfn "%A" ops
    0

let arith (clause : string) =
    let stackOp = []
    let stackNum = []
    let a = getOps clause
    //let stackOp = push a stackOp
    //let a, stackOp = pop stackOp
    //printfn "%s" a
    0

[<EntryPoint>]
let main argv = 
    arith("Add(1,3)")
    Console.ReadLine() |> ignore
    0 // retornar um código de saída inteiro
