module FSheila.Smc

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
        | Number a -> S.Push(Number(a)); aKindOfMagic S M C
        | Id x -> S.Push(M.Item(x)); aKindOfMagic S M C
        | Add (x,y) -> C.Push(CmdAdd); C.Push(y); C.Push(x); aKindOfMagic S M C
        | Assign (a,b) -> S.Push(Id(a)); C.Push(CmdAssign); C.Push(b); aKindOfMagic S M C
        | CmdAdd -> match S.Pop(), S.Pop() with
                    | Number x, Number y -> (S.Push(Number(x + y))) ; aKindOfMagic S M C
                    
        | CmdAssign -> match S.Pop(), S.Pop() with
                      | Number y, Id x -> (M.Add(x,Number y)) ; aKindOfMagic S M C
                      | Boolean y, Id x -> (M.Add(x,Boolean y)) ; aKindOfMagic S M C
                                         
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