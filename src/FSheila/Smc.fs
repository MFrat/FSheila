module FSheila.Smc

open ScanRat
open FSheila
open Utils
open Parser
open System.Collections.Generic

let S = new Stack<Values>()
let M = new Dictionary<string, Values>()
let C = new Stack<Cmd>()


//hello there.
//let calculatorTabajara exp1 exp2 =
//    match exp1 with
//    | Add (x,y) -> match x,y with
//    | Subtract (x,y) 
//    | Multiply (x,y)
//    | Divide (x,y)
//    | And (x,y)
//    | Or of Exp * Exp
//    | Neg of Exp
//    | Eq of  Exp * Exp
//    | Leb of Exp * Exp
//    | Leq of Exp * Exp
//    | Geb of Exp * Exp
//    | Geq of Exp * Exp
//    | Neq of Exp * Exp
let rec aKindOfMagic (S: Stack<Values>) (M: Dictionary<string, Values>) (C: Stack<Cmd>) =
    if C.Count <> 0 then  
        printSMC S M C
        let op = C.Pop()
        match op with
            | Assign (a,b) -> S.Push(Values.Id(a)); C.Push(CmdAssign); match b with
                              | Number x -> S.Push(Values.Number(x)); aKindOfMagic S M C
                              | Add (x,y) -> match x,y with
                                            | Number a, Number b -> S.Push(Values.Number(b));S.Push(Values.Number a); C.Push(CmdAdd) ; aKindOfMagic S M C 
                                            //| a ,b -> 
            //leia o Assign assim como foi feito no Plotkin: ele faz a regra C := I , empilhando o Id da variavel na pilha de valores, a operação de atribuição e a expressão a qual será atribuida                  
            | CmdAdd -> match S.Pop(), S.Pop() with
                        | Values.Number x, Values.Number y -> S.Push(Values.Number(x + y)); aKindOfMagic S M C
                        | a, b -> aKindOfMagic S M C //esse caso nunca vai cair.
            //depois de ter resolvido tudo, o primeiro cara da pilha é um valor (numérico) e abaixo dele é o id:
            | CmdAssign -> match S.Pop(), S.Pop() with
                         | Values.Number y, Values.Id x -> try
                                                             (M.Add(x,Values.Number y))
                                                            with
                                                            | :? System.ArgumentException -> M.Remove(x); M.Add(x,Values.Number y)
                         | Values.Boolean y, Values.Id x -> try
                                                             (M.Add(x,Values.Boolean y))
                                                            with
                                                            | :? System.ArgumentException -> M.Remove(x); M.Add(x,Values.Boolean y)

    
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