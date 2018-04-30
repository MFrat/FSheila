module FSheila.Smc

open ScanRat
open FSheila
open Utils
open Parser
open System.Collections.Generic

let S = new Stack<Values>()
let M = new Dictionary<string, Cmd>()
let C = new Stack<Cmd>()


//hello there.
//até pensei em fazer uma calculadora separada, mas a parte de empilhar na pilha quebra.
let rec aKindOfMagic (S: Stack<Values>) (M: Dictionary<string, Cmd>) (C: Stack<Cmd>) =
    if C.Count <> 0 then  
        printSMC S M C
        let op = C.Pop()
        match op with
            | Assign (a,b) -> S.Push(Values.Id(a)); C.Push(CmdAssign); match b with
                              | Add (x,y) -> match x,y with
                                            | Number a, Number b -> S.Push(Values.Number(b));S.Push(Values.Number a); C.Push(CmdAdd) ; aKindOfMagic S M C
                                            //| a ,b -> 
            //leia o Assign assim como foi feito no Plotkin: ele faz a regra C := I , empilhando o Id da variavel na pilha de valores, a operação de atribuição e a expressão a qual será atribuida                  
            | CmdAdd -> match S.Pop(), S.Pop() with
                        | Values.Number x, Values.Number y -> S.Push(Values.Number(x + y)); aKindOfMagic S M C
                        | a, b -> aKindOfMagic S M C //esse caso nunca vai cair.
             
            

            | Sheila a -> match a with
                            | "Add" -> aKindOfMagic S M C; C.Push(XSheila "Add"); aKindOfMagic S M C
                            | "Subtract" -> aKindOfMagic S M C; C.Push(XSheila "Subtract"); aKindOfMagic S M C
                            | "Multiply" -> aKindOfMagic S M C; C.Push(XSheila "Multiply"); aKindOfMagic S M C
                            | "Divide" -> aKindOfMagic S M C; C.Push(XSheila "Divide"); aKindOfMagic S M C
                            | "And" -> aKindOfMagic S M C; C.Push(XSheila "And"); aKindOfMagic S M C
                            | "Or" -> aKindOfMagic S M C; C.Push(XSheila "Or"); aKindOfMagic S M C
                            | "Neg" -> aKindOfMagic S M C; C.Push(XSheila "Neg"); aKindOfMagic S M C
                            | "Eq" -> aKindOfMagic S M C; C.Push(XSheila "Eq"); aKindOfMagic S M C
                            | "Neq" -> aKindOfMagic S M C; C.Push(XSheila "Neq"); aKindOfMagic S M C
                            | "Leb" -> aKindOfMagic S M C; C.Push(XSheila "Leb"); aKindOfMagic S M C
                            | "Leq" -> aKindOfMagic S M C; C.Push(XSheila "Leq"); aKindOfMagic S M C
                            | "Geb" -> aKindOfMagic S M C; C.Push(XSheila "Geb"); aKindOfMagic S M C
                            | "Geq" -> aKindOfMagic S M C; C.Push(XSheila "Geq"); aKindOfMagic S M C
                            | "Assign" -> aKindOfMagic S M C; C.Push(XSheila "Assign"); aKindOfMagic S M C
            | XSheila a -> match a with
                            | "Add" -> match S.Pop(), S.Pop() with 
                                        | Number x, Number y -> S.Push(Number (x + y))
                                        | Id x, Number y -> match M.Item(x) with
                                                            | Number c -> S.Push(Number(y + c))
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
                            | "Assign" -> match (S.Pop(), S.Pop()) with
                                        | Id x, Number y -> try
                                                             (M.Add(x,Number y))
                                                            with
                                                            | :? System.ArgumentException -> M.Remove(x); M.Add(x,Number y)
                                        | Id x, Boolean y -> (M.Add(x,Boolean y))
                                        | Id x, Id y -> (M.Add(x,M.Item(y)))
            | Number x -> aKindOfMagic S M C; S.Push(Number x)
            | Boolean x -> aKindOfMagic S M C; S.Push(Boolean x)
            | Id x -> aKindOfMagic S M C; S.Push (Id x)

    
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