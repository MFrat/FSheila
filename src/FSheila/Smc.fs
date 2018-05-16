module FSheila.Smc

open ScanRat
open FSheila
open Utils
open Parser
open System.Collections.Generic

type ESMC() =
    member this.E = new Dictionary<string, int>()
    member this.S = new Stack<Cmd>()
    member this.M = new Dictionary<string, Cmd>()
    member this.C = new Stack<Cmd>()
    
    member this.aKindOfMagic =
        if this.C.Count <> 0 then  
            printSMC this.S this.M this.C
            let op = this.C.Pop()
            match op with
            //Default cases
            | Number x -> this.S.Push(Number(x))
            | Boolean x -> this.S.Push(Boolean(x))
            | Id x -> this.S.Push(this.M.Item(x))
            //Operations
            | Add (x,y) -> this.C.Push(CmdAdd); this.C.Push(y); this.C.Push(x)
            | Subtract (x,y) -> this.C.Push(CmdSubtract); this.C.Push(y); this.C.Push(x)
            | Multiply (x,y) -> this.C.Push(CmdMultiply); this.C.Push(y); this.C.Push(x)
            | Divide (x,y) -> this.C.Push(CmdDivide); this.C.Push(y); this.C.Push(x)
            | And (x,y) -> this.C.Push(CmdAnd); this.C.Push(y); this.C.Push(x)
            | Or (x,y) -> this.C.Push(CmdOr); this.C.Push(y); this.C.Push(x)
            | Neg x -> this.C.Push(CmdNeg); this.C.Push(x)
            | Eq (x,y) -> this.C.Push(CmdEq); this.C.Push(y); this.C.Push(x)
            | Neq (x,y) -> this.C.Push(CmdNeq); this.C.Push(y); this.C.Push(x)
            | Leb (x,y) -> this.C.Push(CmdLeb); this.C.Push(y); this.C.Push(x)
            | Leq (x,y) -> this.C.Push(CmdLeq); this.C.Push(y); this.C.Push(x)
            | Geb (x,y) -> this.C.Push(CmdGeb); this.C.Push(y); this.C.Push(x)
            | Geq (x,y) -> this.C.Push(CmdGeq); this.C.Push(y); this.C.Push(x)
            //Commands
            | Assign (x,y) -> this.S.Push(Id(x)); this.C.Push(CmdAssign); this.C.Push(y)
            | If (x,y,z) -> this.S.Push(z); this.S.Push(y); this.C.Push(CmdIf); this.C.Push(x)
            | Loop (x,y) -> this.S.Push(y); this.S.Push(x); this.C.Push(CmdLoop); this.C.Push(x)
            | Seq (x,y) -> this.C.Push(y); this.C.Push(x)
            //Actions
            | CmdAdd -> match this.S.Pop(), this.S.Pop() with
                        | Number x, Number y -> (this.S.Push(Number(x + y)))
            | CmdSubtract -> match this.S.Pop(), this.S.Pop() with
                        | Number x, Number y -> (this.S.Push(Number(y - x)))
            | CmdMultiply -> match this.S.Pop(), this.S.Pop() with
                        | Number x, Number y -> (this.S.Push(Number(x * y)))
            | CmdDivide -> match this.S.Pop(), this.S.Pop() with
                        | Number x, Number y -> (this.S.Push(Number(y / x)))
            | CmdAnd -> match this.S.Pop(), this.S.Pop() with
                        | Boolean x, Boolean y -> (this.S.Push(Boolean(x && y)))
            | CmdOr -> match this.S.Pop(), this.S.Pop() with
                        | Boolean x, Boolean y -> (this.S.Push(Boolean(x || y)))
            | CmdNeg -> match this.S.Pop() with
                        | Boolean x -> (this.S.Push(Boolean(not(x))))
            | CmdEq -> match this.S.Pop(), this.S.Pop() with
                        | Boolean x, Boolean y -> (this.S.Push(Boolean(x = y)))
                        | Number x, Number y -> (this.S.Push(Boolean(x = y)))             
            | CmdNeq -> match this.S.Pop(), this.S.Pop() with
                        | Boolean x, Boolean y -> (this.S.Push(Boolean(x <> y)))
                        | Number x, Number y -> (this.S.Push(Boolean(x <> y)))                
            | CmdLeb -> match this.S.Pop(), this.S.Pop() with
                        | Number x, Number y -> (this.S.Push(Boolean(y < x)))
            | CmdLeq -> match this.S.Pop(), this.S.Pop() with
                        | Number x, Number y -> (this.S.Push(Boolean(y <= x)))
            | CmdGeb -> match this.S.Pop(), this.S.Pop() with
                        | Number x, Number y -> (this.S.Push(Boolean(y > x)))
            | CmdGeq -> match this.S.Pop(), this.S.Pop() with
                        | Number x, Number y -> (this.S.Push(Boolean(y >= x)))
            | CmdAssign -> match this.S.Pop(), this.S.Pop() with
                            | Number y, Id x -> try (this.M.Add(x,Number y))
                                                with
                                                | :? System.ArgumentException -> this.M.Remove(x) ; this.M.Add(x,Number y)
                            | Boolean y, Id x -> try (this.M.Add(x,Boolean y))
                                                 with
                                                 | :? System.ArgumentException -> this.M.Remove(x)  ; this.M.Add(x,Boolean y)
                            | Id y, Id x      -> try (this.M.Add(y,this.M.Item(x)))
                                                 with
                                                 | :? System.ArgumentException -> this.M.Remove(x)  ; (this.M.Add(y,this.M.Item(x)))
            | CmdIf -> match this.S.Pop(), this.S.Pop(), this.S.Pop() with
                        | Boolean x, y, z -> match x with
                            | true -> this.C.Push(y)
                            | false -> this.C.Push(z)
            | CmdLoop -> match this.S.Pop(), this.S.Pop(), this.S.Pop() with
                        | Boolean x, y, z -> match x with
                            | true -> this.C.Push(Loop(y,z)); this.C.Push(z)
                            | false -> ()
            ; this.aKindOfMagic
        
let stackator (cmd : Cmd) (eSMC : ESMC) =
    eSMC.C.Push(cmd)

let getFromParser (exp) (eSMC : ESMC) =
    match exp with
    | Success r -> printfn "Input = %A" r.value; stackator r.value eSMC
    | Failure _ -> failwith "Parsing falhou!"