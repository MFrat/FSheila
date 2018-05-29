module FSheila.Smc

open ScanRat
open FSheila
open Utils
open Parser
open System.Collections.Generic

type ESMC() =   
    let mutable E = new Dictionary<string, Cmd>() //ACORDO DE CAVALHEIROS: A STRING TEM QUE SER UM ID, PAU NO CU!!!!
    let mutable S = new Stack<Cmd>()
    let mutable M = new Dictionary<Cmd, Cmd>() //ACORDO DE CAVALHEIROS: O PRIMEIRO CMD TEM QUE SER UM NUMBER, PAU NO CU!!!!
    let mutable C = new Stack<Cmd>()

    member this.fillMemory =
        //E.Add("teste", Number(bigint -1))
        M.Add(Id("y"), Number(bigint 1))
        M.Add(Id("x"), Number(bigint 10))
        M.Add(Id("sheila"),Boolean(false))

    member this.fillController (bplc : Cmd) = 
        C.Push(bplc)

    member this.print = 
        printfn "E = %A" (E)
        printfn "S = %A" (S)
        printfn "M = %A" (M)
        printfn "C = %A" (C)
        printfn "\n"
    
    member private this.newEnviroment =
        new Dictionary<string, Cmd>(E)
    
    //seta o elemento na memória e retorna o location
    member private this.setOnMemory (c : Cmd) =
        //ATENCAO TA ERRADO ESSA LOCATION, TEM QUE USAR O TIPO LOCATION, MAS TO COM SONO ENTAO VAI FICAR ASSIM POR ENQUANTO
        let location = Number(bigint -1)
        for i in 0 .. M.Keys.Count do
            let idx = Number(bigint i)
            match not (M.ContainsKey(idx)) with
            | true -> location = idx; M.Add(idx, c)

        if location = Number(bigint -1) then
            location = Number(bigint M.Keys.Count)
            M.Add(location, c)
            location
        else
            location

    member this.enviroment (op : Cmd) = 
        match op with
        | ConstBlock (x,y) -> S.Push(CmdDict(this.newEnviroment)); match x,y with
            | ConstInit (a,b), y ->
            match b with
                | Id c -> E.Add(string(a),Id(c))
                | Number c -> E.Add(string(a), Number(c))
                | Boolean c -> E.Add(string(a), Boolean(c))
            ; C.Push(CmdConstBlock); C.Push(y)
        | CmdConstBlock -> match S.Pop() with
            | CmdDict x -> E <- x
        | VarBlock (x,y) -> S.Push(CmdDict(this.newEnviroment)); match x,y with
            | VarInit (a,b), y ->
            match b with
                | Id c -> E.Add(string(a), this.setOnMemory(Id(c)))
                | Number c -> E.Add(string(a), this.setOnMemory(Number(c)))
                | Boolean c -> E.Add(string(a), this.setOnMemory(Boolean(c)))
            ; C.Push(CmdVarBlock); C.Push(y)
        | CmdVarBlock -> match S.Pop() with //ALEM DISSO PRECISA LIMPAR A MEMORIA
            | CmdDict x -> E <- x

    member this.aKindOfMagic =
        if C.Count <> 0 then  
            this.print
            let op = C.Pop()
            match op with
            //Default cases
            | Number x -> S.Push(Number(x))
            | Boolean x -> S.Push(Boolean(x))
            | Id x -> S.Push(M.Item(Id(x)))
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
            | Loop (x,y) -> S.Push(y); S.Push(x); C.Push(CmdLoop); C.Push(x)
            | Seq (x,y) -> C.Push(y); C.Push(x)
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
                        | Number x, Number y -> (S.Push(Boolean(y < x)))
            | CmdLeq -> match S.Pop(), S.Pop() with
                        | Number x, Number y -> (S.Push(Boolean(y <= x)))
            | CmdGeb -> match S.Pop(), S.Pop() with
                        | Number x, Number y -> (S.Push(Boolean(y > x)))
            | CmdGeq -> match S.Pop(), S.Pop() with
                        | Number x, Number y -> (S.Push(Boolean(y >= x)))
            | CmdAssign -> match S.Pop(), S.Pop() with
                            | Number y, Id x -> try (M.Add(Id(x),Number y))
                                                with
                                                | :? System.ArgumentException -> M.Remove(Id(x)) ; M.Add(Id(x),Number y)
                            | Boolean y, Id x -> try (M.Add(Id(x),Boolean y))
                                                 with
                                                 | :? System.ArgumentException -> M.Remove(Id(x))  ; M.Add(Id(x),Boolean y)
                            | Id y, Id x      -> try (M.Add(Id(y),M.Item(Id(x))))
                                                 with
                                                 | :? System.ArgumentException -> M.Remove(Id(x))  ; (M.Add(Id(y),M.Item(Id(x))))
            | CmdIf -> match S.Pop(), S.Pop(), S.Pop() with
                        | Boolean x, y, z -> match x with
                            | true -> C.Push(y)
                            | false -> C.Push(z)
            | CmdLoop -> match S.Pop(), S.Pop(), S.Pop() with
                        | Boolean x, y, z -> match x with
                            | true -> C.Push(Loop(y,z)); C.Push(z)
                            | false -> ()
            | _ -> this.enviroment op
            ; this.aKindOfMagic

let stackator (cmd : Cmd) (eSMC : ESMC) =
    eSMC.fillController(cmd)

let getFromParser (exp) (eSMC : ESMC) =
    match exp with
    | Success r -> printfn "Input = %A" r.value; stackator r.value eSMC
    | Failure f -> printfn "%A" f.index //failwith "Parsing falhou!"