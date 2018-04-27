module FSheila.Smc

open ScanRat
open FSheila
open Utils
open Parser
open System.Collections.Generic

let S = new Stack<Cmd>()
let M = new Dictionary<string, Cmd>()
let C = new Stack<Cmd>()

let rec new_sum (S: Stack<Cmd>) (C: Stack<Cmd>) =
    if C.Count <> 0 then  
        printStacks S C
        let op = C.Pop()
        match op with
            | Sheila a -> match a with
                            | "Add" -> new_sum S C; C.Push(XSheila "Add"); new_sum S C
                            | "Subtract" -> new_sum S C; C.Push(XSheila "Subtract"); new_sum S C
                            | "Multiply" -> new_sum S C; C.Push(XSheila "Multiply"); new_sum S C
                            | "Divide" -> new_sum S C; C.Push(XSheila "Divide"); new_sum S C
                            | "And" -> new_sum S C; C.Push(XSheila "And"); new_sum S C
                            | "Or" -> new_sum S C; C.Push(XSheila "Or"); new_sum S C
                            | "Neg" -> new_sum S C; C.Push(XSheila "Neg"); new_sum S C
                            | "Eq" -> new_sum S C; C.Push(XSheila "Eq"); new_sum S C
                            | "Neq" -> new_sum S C; C.Push(XSheila "Neq"); new_sum S C
                            | "Leb" -> new_sum S C; C.Push(XSheila "Leb"); new_sum S C
                            | "Leq" -> new_sum S C; C.Push(XSheila "Leq"); new_sum S C
                            | "Geb" -> new_sum S C; C.Push(XSheila "Geb"); new_sum S C
                            | "Geq" -> new_sum S C; C.Push(XSheila "Geq"); new_sum S C
            | XSheila a -> match a with
                            | "Add" -> match S.Pop(), S.Pop() with 
                                        | Number x, Number y -> S.Push(Number (x + y))
                            | "Subtract" -> match S.Pop(), S.Pop() with 
                                        | Number x, Number y -> S.Push(Number (x - y))
                            | "Multiply" -> match S.Pop(), S.Pop() with 
                                        | Number x, Number y -> S.Push(Number (x * y))
                            | "Divide" -> match S.Pop(), S.Pop() with 
                                        | Number x, Number y -> S.Push(Number (x / y))
                            | "And" -> match S.Pop(), S.Pop() with 
                                        | Boolean x, Boolean y -> S.Push(Boolean(x && y))
                            | "Or" -> match S.Pop(), S.Pop() with 
                                        | Boolean x, Boolean y -> S.Push(Boolean(x || y))
                            | "Neg" -> match S.Pop() with 
                                        | Boolean x -> S.Push(Boolean(not(x)))
                            | "Eq" -> match S.Pop(), S.Pop() with 
                                        | Boolean x, Boolean y -> S.Push(Boolean(x = y))
                                        | Number x, Number y -> S.Push(Boolean(x = y))
                            | "Neq" -> match S.Pop(), S.Pop() with 
                                        | Boolean x, Boolean y -> S.Push(Boolean(x <> y))
                                        | Number x, Number y -> S.Push(Boolean(x <> y))
                            | "Leb" -> match S.Pop(), S.Pop() with 
                                        | Boolean x, Boolean y -> S.Push(Boolean(x < y))
                            | "Leq" -> match S.Pop(), S.Pop() with 
                                        | Boolean x, Boolean y -> S.Push(Boolean(x <= y))
                            | "Geb" -> match S.Pop(), S.Pop() with 
                                        | Boolean x, Boolean y -> S.Push(Boolean(x > y))
                            | "Geq" -> match S.Pop(), S.Pop() with 
                                        | Boolean x, Boolean y -> S.Push(Boolean(x >= y))
            | Number x -> new_sum S C; S.Push(Number x)
            | Boolean x -> new_sum S C; S.Push(Boolean x)
            
let rec stackator (exp) =
    match exp with
    //essas três primeiras representam o "caso base" de algumas regras.
    | Number a -> S.Push(Number a)
    | Boolean b -> S.Push (Boolean b)
    | Id x -> S.Push (M.Item(x)) //quando stackator acerta um Id puro, significa que alguém está referenciando essa variável (ex a := sheila ou sheila + sheila2 - 5). Nesse caso, o que deve ser empilhado é o valor referente ao id na memória.
    | Add (a, b) -> match (a,b) with
                    | Number x, Number y -> C.Push(Number y); C.Push(Number x); C.Push(Sheila "Add")
                    | Number x , d -> stackator d; C.Push(Number x); C.Push(Sheila "Add")
                    | d , Number y ->  C.Push(Number y); stackator d; C.Push(Sheila "Add")
                    | v , k -> stackator (v); stackator (k); C.Push(Sheila "Add")
    | Subtract (a, b) -> match (a,b) with
                    | Number x, Number y -> C.Push(Number y); C.Push(Number x); C.Push(Sheila "Subtract")
                    | Number x , d -> stackator d; C.Push(Number x); C.Push(Sheila "Subtract")
                    | d , Number y ->  C.Push(Number y); stackator d; C.Push(Sheila "Subtract")
                    | v , k -> stackator (v); stackator (k); C.Push(Sheila "Subtract")
    | Multiply (a, b) -> match (a,b) with
                    | Number x, Number y -> C.Push(Number y); C.Push(Number x); C.Push(Sheila "Multiply")
                    | Number x , d -> stackator d; C.Push(Number x); C.Push(Sheila "Multiply")
                    | d , Number y ->  C.Push(Number y); stackator d; C.Push(Sheila "Multiply")
                    | v , k -> stackator (v); stackator (k); C.Push(Sheila "Multiply")
    | Divide (a, b) -> match (a,b) with
                    | Number x, Number y -> C.Push(Number y); C.Push(Number x); C.Push(Sheila "Divide")
                    | Number x , d -> stackator d; C.Push(Number x); C.Push(Sheila "Divide")
                    | d , Number y ->  C.Push(Number y); stackator d; C.Push(Sheila "Divide")
                    | v , k -> stackator (v); stackator (k); C.Push(Sheila "Divide")
    | And (a, b) -> match a,b with
                    | Boolean x, Boolean y -> C.Push(Boolean y); C.Push(Boolean x); C.Push(Sheila "And")
                    | Boolean x , d -> stackator d; C.Push(Boolean x); C.Push(Sheila "And")
                    | d , Boolean y -> S.Push (Boolean y); stackator d; C.Push(Sheila "And")
                    | v , k -> stackator (v); stackator (k); C.Push(Sheila "And")
    | Or (a, b) -> match a,b with
                    | Boolean x, Boolean y -> S.Push(Boolean y); S.Push(Boolean x); C.Push(Sheila "Or")
                    | Boolean x , d -> stackator d; S.Push(Boolean x); C.Push(Sheila "Or")
                    | d , Boolean y -> S.Push (Boolean y); stackator d; C.Push(Sheila "Or")
                    | v , k -> stackator (v); stackator (k); C.Push(Sheila "Or")
    | Neg a -> match a with 
                    | Boolean x -> C.Push(Boolean x); C.Push(Sheila "Neg")
                    | d -> stackator(d)
    | Eq (a, b) -> match a,b with
                    | Boolean x, Boolean y -> S.Push(Boolean y); S.Push(Boolean x); C.Push(Sheila "Eq")
                    | Boolean x , d -> stackator d; S.Push(Boolean x); C.Push(Sheila "Eq")
                    | d , Boolean y -> S.Push (Boolean y); stackator d; C.Push(Sheila "Eq")
                    | v , k -> stackator (v); stackator (k); C.Push(Sheila "Eq")
    | Neq (a, b) -> match a,b with
                    | Boolean x, Boolean y -> S.Push(Boolean y); S.Push(Boolean x); C.Push(Sheila "Neq")
                    | Boolean x , d -> stackator d; S.Push(Boolean x); C.Push(Sheila "Neq")
                    | d , Boolean y -> S.Push (Boolean y); stackator d; C.Push(Sheila "Neq")
                    | v , k -> stackator (v); stackator (k); C.Push(Sheila "Neq")
    | Leb (a, b) -> match a,b with
                    | Boolean x, Boolean y -> S.Push(Boolean y); S.Push(Boolean x); C.Push(Sheila "Leb")
                    | Boolean x , d -> stackator d; S.Push(Boolean x); C.Push(Sheila "Leb")
                    | d , Boolean y -> S.Push (Boolean y); stackator d; C.Push(Sheila "Leb")
                    | v , k -> stackator (v); stackator (k); C.Push(Sheila "Leb")
    | Leq (a, b) -> match a,b with
                    | Boolean x, Boolean y -> S.Push(Boolean y); S.Push(Boolean x); C.Push(Sheila "Leq")
                    | Boolean x , d -> stackator d; S.Push(Boolean x); C.Push(Sheila "Leq")
                    | d , Boolean y -> S.Push (Boolean y); stackator d; C.Push(Sheila "Leq")
                    | v , k -> stackator (v); stackator (k); C.Push(Sheila "Leq")
    | Geb (a, b) -> match a,b with
                    | Boolean x, Boolean y -> S.Push(Boolean y); S.Push(Boolean x); C.Push(Sheila "Geb")
                    | Boolean x , d -> stackator d; S.Push(Boolean x); C.Push(Sheila "Geb")
                    | d , Boolean y -> S.Push (Boolean y); stackator d; C.Push(Sheila "Geb")
                    | v , k -> stackator (v); stackator (k); C.Push(Sheila "Geb")
    | Geq (a, b) -> match a,b with
                    | Boolean x, Boolean y -> S.Push(Boolean y); S.Push(Boolean x); C.Push(Sheila "Geq")
                    | Boolean x , d -> stackator d; S.Push(Boolean x); C.Push(Sheila "Geq")
                    | d , Boolean y -> S.Push (Boolean y); stackator d; C.Push(Sheila "Geq")
                    | v , k -> stackator (v); stackator (k); C.Push(Sheila "Geq")
    
    (*
    //comandos TODO Assign, While e If.
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

let getFromParser (exp) =
    match exp with
    | Success r -> printfn "Input = %A" r.value; stackator r.value
    | Failure _ -> failwith "Parsing falhou!"