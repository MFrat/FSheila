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
        //Default cases
        | Number x -> S.Push(Number(x)); aKindOfMagic S M C
        | Boolean x -> S.Push(Boolean(x)); aKindOfMagic S M C
        | Id x -> S.Push(M.Item(x)); aKindOfMagic S M C
        //Operations
        | Add (x,y) -> C.Push(CmdAdd); C.Push(y); C.Push(x); aKindOfMagic S M C
        | Subtract (x,y) -> C.Push(CmdSubtract); C.Push(y); C.Push(x); aKindOfMagic S M C
        | Multiply (x,y) -> C.Push(CmdMultiply); C.Push(y); C.Push(x); aKindOfMagic S M C
        | Divide (x,y) -> C.Push(CmdDivide); C.Push(y); C.Push(x); aKindOfMagic S M C
        | And (x,y) -> C.Push(CmdAnd); C.Push(y); C.Push(x); aKindOfMagic S M C
        | Or (x,y) -> C.Push(CmdOr); C.Push(y); C.Push(x); aKindOfMagic S M C
        | Neg x -> C.Push(CmdNeg); C.Push(x); aKindOfMagic S M C
        | Eq (x,y) -> C.Push(CmdEq); C.Push(y); C.Push(x); aKindOfMagic S M C
        | Neq (x,y) -> C.Push(CmdNeq); C.Push(y); C.Push(x); aKindOfMagic S M C
        | Leb (x,y) -> C.Push(CmdLeb); C.Push(y); C.Push(x); aKindOfMagic S M C
        | Leq (x,y) -> C.Push(CmdLeq); C.Push(y); C.Push(x); aKindOfMagic S M C
        | Geb (x,y) -> C.Push(CmdGeb); C.Push(y); C.Push(x); aKindOfMagic S M C
        | Geq (x,y) -> C.Push(CmdGeq); C.Push(y); C.Push(x); aKindOfMagic S M C
        | Assign (x,y) -> S.Push(Id(x)); C.Push(CmdAssign); C.Push(y); aKindOfMagic S M C
        //Actions
        | CmdAdd -> match S.Pop(), S.Pop() with
                    | Number x, Number y -> (S.Push(Number(x + y))) ; aKindOfMagic S M C
        | CmdSubtract -> match S.Pop(), S.Pop() with
                    | Number x, Number y -> (S.Push(Number(y - x))) ; aKindOfMagic S M C
        | CmdMultiply -> match S.Pop(), S.Pop() with
                    | Number x, Number y -> (S.Push(Number(x * y))) ; aKindOfMagic S M C
        | CmdDivide -> match S.Pop(), S.Pop() with
                    | Number x, Number y -> (S.Push(Number(y / x))) ; aKindOfMagic S M C
        | CmdAnd -> match S.Pop(), S.Pop() with
                    | Boolean x, Boolean y -> (S.Push(Boolean(x && y))) ; aKindOfMagic S M C
        | CmdOr -> match S.Pop(), S.Pop() with
                    | Boolean x, Boolean y -> (S.Push(Boolean(x || y))) ; aKindOfMagic S M C
        | CmdNeg -> match S.Pop() with
                    | Boolean x -> (S.Push(Boolean(not(x)))) ; aKindOfMagic S M C
        | CmdEq -> match S.Pop(), S.Pop() with
                    | Boolean x, Boolean y -> (S.Push(Boolean(x = y))) ; aKindOfMagic S M C
        | CmdNeq -> match S.Pop(), S.Pop() with
                    | Boolean x, Boolean y -> (S.Push(Boolean(x <> y))) ; aKindOfMagic S M C
        | CmdLeb -> match S.Pop(), S.Pop() with
                    | Boolean x, Boolean y -> (S.Push(Boolean(x < y))) ; aKindOfMagic S M C
        | CmdLeq -> match S.Pop(), S.Pop() with
                    | Boolean x, Boolean y -> (S.Push(Boolean(x <= y))) ; aKindOfMagic S M C
        | CmdGeb -> match S.Pop(), S.Pop() with
                    | Boolean x, Boolean y -> (S.Push(Boolean(x > y))) ; aKindOfMagic S M C
        | CmdGeq -> match S.Pop(), S.Pop() with
                    | Boolean x, Boolean y -> (S.Push(Boolean(x >= y))) ; aKindOfMagic S M C
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