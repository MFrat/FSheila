﻿module FSheila.Smc

open ScanRat
open FSheila
open Utils
open Parser
open System.Collections.Generic

let S = new Stack<Cmd>()
let M = new Dictionary<string, Cmd>()
let C = new Stack<Cmd>()



let rec aKindOfMagic (S: Stack<Cmd>) (M: Dictionary<string, Cmd>) (C: Stack<Cmd>) =
    if C.Count <> 0 then  
        printSMC S M C
        let op = C.Pop()
        match op with
        //Default cases
        | Number x -> S.Push(Number(x))
        | Boolean x -> S.Push(Boolean(x))
        | Id x -> S.Push(M.Item(x))
        //Operations
        | Add (x,y) -> C.Push(CmdAdd); C.Push(y); C.Push(x)
        | Subtract (x,y) -> C.Push(CmdSubtract); C.Push(y); C.Push(x)
        | Multiply (x,y) -> C.Push(CmdMultiply); C.Push(y); C.Push(x)
        | Divide (x,y) -> C.Push(CmdDivide); C.Push(y); C.Push(x)
        | And (x,y) -> C.Push(CmdAnd); C.Push(y); C.Push(x)
        | Or (x,y) -> C.Push(CmdOr); C.Push(y); C.Push(x)
        | Neg x -> C.Push(CmdNeg); C.Push(x)
        | Eq (x,y) -> C.Push(CmdEq); C.Push(y); C.Push(x)
        | Neq (x,y) -> C.Push(CmdNeq); C.Push(y); C.Push(x)
        | Leb (x,y) -> C.Push(CmdLeb); C.Push(y); C.Push(x)
        | Leq (x,y) -> C.Push(CmdLeq); C.Push(y); C.Push(x)
        | Geb (x,y) -> C.Push(CmdGeb); C.Push(y); C.Push(x)
        | Geq (x,y) -> C.Push(CmdGeq); C.Push(y); C.Push(x)
        //Commands
        | Assign (x,y) -> S.Push(Id(x)); C.Push(CmdAssign); C.Push(y)
        | If (x,y,z) -> S.Push(z); S.Push(y); C.Push(CmdIf); C.Push(x)
        | Loop (x,y) -> S.Push(y); S.Push(x); S.Push(CmdLoop); C.Push(x)
        //| Seq ->
        //Actions
        | CmdAdd -> match S.Pop(), S.Pop() with
                    | Number x, Number y -> (S.Push(Number(x + y)))
        | CmdSubtract -> match S.Pop(), S.Pop() with
                    | Number x, Number y -> (S.Push(Number(y - x)))
        | CmdMultiply -> match S.Pop(), S.Pop() with
                    | Number x, Number y -> (S.Push(Number(x * y)))
        | CmdDivide -> match S.Pop(), S.Pop() with
                    | Number x, Number y -> (S.Push(Number(y / x)))
        | CmdAnd -> match S.Pop(), S.Pop() with
                    | Boolean x, Boolean y -> (S.Push(Boolean(x && y)))
        | CmdOr -> match S.Pop(), S.Pop() with
                    | Boolean x, Boolean y -> (S.Push(Boolean(x || y)))
        | CmdNeg -> match S.Pop() with
                    | Boolean x -> (S.Push(Boolean(not(x))))
        | CmdEq -> match S.Pop(), S.Pop() with
                    | Boolean x, Boolean y -> (S.Push(Boolean(x = y)))
                    | Number x, Number y -> (S.Push(Boolean(x = y)))             
        | CmdNeq -> match S.Pop(), S.Pop() with
                    | Boolean x, Boolean y -> (S.Push(Boolean(x <> y)))
                    | Number x, Number y -> (S.Push(Boolean(x <> y)))                
        | CmdLeb -> match S.Pop(), S.Pop() with
                    | Number x, Number y -> (S.Push(Boolean(x < y)))
        | CmdLeq -> match S.Pop(), S.Pop() with
                    | Number x, Number y -> (S.Push(Boolean(x <= y)))
        | CmdGeb -> match S.Pop(), S.Pop() with
                    | Number x, Number y -> (S.Push(Boolean(x > y)))
        | CmdGeq -> match S.Pop(), S.Pop() with
                    | Number x, Number y -> (S.Push(Boolean(x >= y)))
        | CmdAssign -> match S.Pop(), S.Pop() with
                        | Number y, Id x -> (M.Add(x,Number y))
                        | Boolean y, Id x -> (M.Add(x,Boolean y)) 
        | CmdIf -> match S.Pop(), S.Pop(), S.Pop() with
                    | Boolean x, y, z -> match x with
                        | true -> C.Push(y)
                        | false -> C.Push(z)
        | CmdLoop -> match S.Pop(), S.Pop(), S.Pop() with
                    | Boolean x, y, z -> match x with
                        | true -> C.Push(Loop(y,z)); C.Push(z)
                        | false -> ()
        ; aKindOfMagic S M C
        
                                         
    (*
    //comandos TODO While e If.
    | Assign (a,b) -> X.Push("Assign"); S.Push(Id a); stackator b
    | If (a,b,c) -> X.Push("If") ; S.Push(c); S.Push(b); S.Push(a) //IDEIA: segundo plotkin, empulha os comandos todos na pilha S. Se a for verdade, executar b e desempilhar c, se a não for verdade, desempilha b e executa c.
    | Loop (a,b) -> X.Push("Loop") ; S.Push(a); S.Push(b) //a semántica das regras de eliminação E1 e E2 do plotkin virão das calculadoras
    //real if e while não sei fazer direito não.

//let commandCalculator  (X: Stack<string>) (S: Stack<Cmd>) =
//    while X.Count <> 0 do
//       let op = X.Pop()
//        let d1 = S.Pop()
//        let d2 = S.Peek() //famosa gambiarra
//        match op with
//            | "Assign" -> match (d1,d2) with //queria reutilizar uma possivel calculadora, mas vamos ver:
//                         | Id a, k -> match k with //BUGADO: necessitamos de uma forma de resolver k antes de fazer M.Item(a,k), ou seja, atribuir k ao valor "a" na memória M.                                        
    
    *)
let stackator cmd = C.Push(cmd)
let getFromParser (exp) =
    match exp with
    | Success r -> printfn "Input = %A" r.value; stackator r.value
    | Failure _ -> failwith "Parsing falhou!"