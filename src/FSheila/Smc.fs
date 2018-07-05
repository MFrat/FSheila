module FSheila.Smc

open ScanRat
open FSheila
open Utils
open Parser
open System.Collections.Generic

type ESMC() =   
    let mutable E = new Dictionary<string, Tipao>() //ACORDO DE CAVALHEIROS: A STRING TEM QUE SER UM ID E Tipao UM LOCATION (se for var) ou Tipao msm
    let mutable S = new Stack<Tipao>()
    let mutable M = new Dictionary<Tipao, Tipao>() //ACORDO DE CAVALHEIROS: O PRIMEIRO Tipao TEM QUE SER UM LOCATION
    let mutable C = new Stack<Tipao>()

    member this.fillEnviroment = 
        E.Add("teste", Location(int 10))
        M.Add(Location(int 10), Number(bigint 1))

    member this.fillController (bplc : Tipao) = 
        C.Push(bplc)

    member this.print = 
        printfn "E = %A" (E)
        printfn "S = %A" (S)
        printfn "M = %A" (M)
        printfn "C = %A" (C)
        printfn "\n"
    
    member private this.newEnviroment =
        new Dictionary<string, Tipao>(E)
    
    //seta o elemento na memória e retorna o location
    member private this.setOnMemory (c : Tipao) =
        let mutable location = Location(int -1)
        for i in 0 .. M.Keys.Count do
            let idx = Location(int i)
            match (M.ContainsKey(idx)) with
            | false ->
                location <- idx
                M.Add(location, c)
            | _ -> ()
        
        if location = Location(int -1) then
            location <- Location(int M.Keys.Count)
            M.Add(location, c)
        
        location

    member private this.garbageCollector =
        for entry in Dictionary<Tipao, Tipao>(M) do //faz uma cópia da memória para iterar
            match entry.Key with
            | Location l -> match E.ContainsValue(Location l) with
                | false -> M.Remove(Location l) |> ignore
                | _ -> ()

    member private this.findIdValue (id : string) = 
        match E.Item(id) with
        | Location x -> M.Item(Location x)
        | Number x -> Number x
        | Boolean x -> Boolean x
        | _ -> failwith "Id not found"
    
    member this.enviromentStuff (op : Tipao) = 
        match op with
        | ConstBlock (x,y) -> S.Push(Enviroment(this.newEnviroment)); match x,y with
            | ConstInit (a,b), y ->
            match b with
                | Id b -> E.Add(string(a), this.findIdValue(b))
                | Number b -> E.Add(string(a), Number(b))
                | Boolean b -> E.Add(string(a), Boolean(b))
            ; C.Push(XConstBlock); C.Push(y)
        | XConstBlock -> match S.Pop() with
            | Enviroment x -> E <- x
        | VarBlock (x,y) -> S.Push(Enviroment(this.newEnviroment)); match x,y with
            | VarInit (a,b), y ->
            match b with
                | Id b -> E.Add(string(a), this.setOnMemory(Id(b)))
                | Number b -> E.Add(string(a), this.setOnMemory(Number(b)))
                | Boolean b -> E.Add(string(a), this.setOnMemory(Boolean(b)))
            ; C.Push(XVarBlock); C.Push(y)
        | XVarBlock -> match S.Pop() with //ALEM DISSO PRECISA LIMPAR A MEMORIA
            | Enviroment x -> E <- x; this.garbageCollector
        | _ -> failwith "Deu ruim"
    
    member private this.checkVarType (id : Tipao) =
        match id with
        | Id x -> match E.Item(x) with
            | Location x -> id
            | _ -> printfn "(DummyUserError) You shall not assign a const"; failwith ""
        | _ -> printfn "(DummyUserError) You shall declare yours variables"; failwith ""
   
    member private this.addFunProc (id : Tipao) (abs : Tipao) = 
        match id with
        | Id x -> match abs with
            | Abs (y,z) -> E.Add(string x, Abs(y,z))
            | Absf (z) -> E.Add(string x, Absf(z))
    
    member private this.FunPrc (id : Tipao) (actuals : Tipao) = 
        match id with
        | Id x -> match E.Item(x) with
        //y=formals;z=blk  
            | Abs (y,z) -> this.matchFormals y actuals; C.Push(z)
            | Absf z -> S.Push(Enviroment(this.newEnviroment)); C.Push(z)
    
    member private this.matchFormals (formals : Tipao) (actuals : Tipao) =
        // falta fazer para o caso de multiplos parametros
        S.Push(Enviroment(this.newEnviroment))
        // for f,a in List.zip formals actuals do
        // se for constante
        // match formals, actuals with
        //    | For f, Act a -> match a with
        //        | Id a -> E.Add(string(f), this.findIdValue(a))
        //        | Number a -> E.Add(string(f), Number(a))
        //        | Boolean a -> E.Add(string(f), Boolean(a))
        // se for variável
        match formals, actuals with
           | For f, Act a -> match f, a with
               | Id f, Id a -> E.Add(string f, this.setOnMemory(Id(a)))
               | Id f, Number a -> E.Add(string f, this.setOnMemory(Number(a)))
               | Id f, Boolean a -> E.Add(string f, this.setOnMemory(Boolean(a)))

    member this.aKindOfMagic =
        if C.Count <> 0 then  
            this.print
            let op = C.Pop()
            match op with
            //Default cases
            | Number x -> S.Push(Number(x))
            | Boolean x -> S.Push(Boolean(x))
            | Id x -> match E.Item(string x) with 
                | Location y -> S.Push(M.Item(Location y))
                | Abs (y,z) -> S.Push(Id(x))
                | Absf y -> S.Push(Id(x))
                | _ -> S.Push(E.Item(string x))
            //Operations
            | Add (x,y) -> C.Push(XAdd); C.Push(y); C.Push(x)
            | Subtract (x,y) -> C.Push(XSubtract); C.Push(y); C.Push(x)
            | Multiply (x,y) -> C.Push(XMultiply); C.Push(y); C.Push(x)
            | Divide (x,y) -> C.Push(XDivide); C.Push(y); C.Push(x)
            | And (x,y) -> C.Push(XAnd); C.Push(y); C.Push(x)
            | Or (x,y) -> C.Push(XOr); C.Push(y); C.Push(x)
            | Neg x -> C.Push(XNeg); C.Push(x)
            | Eq (x,y) -> C.Push(XEq); C.Push(y); C.Push(x)
            | Neq (x,y) -> C.Push(XNeq); C.Push(y); C.Push(x)
            | Leb (x,y) -> C.Push(XLeb); C.Push(y); C.Push(x)
            | Leq (x,y) -> C.Push(XLeq); C.Push(y); C.Push(x)
            | Geb (x,y) -> C.Push(XGeb); C.Push(y); C.Push(x)
            | Geq (x,y) -> C.Push(XGeq); C.Push(y); C.Push(x)
            //Commands
            | Assign (x,y) -> S.Push(Id(x)); C.Push(XAssign); C.Push(y)
            | If (x,y,z) -> S.Push(z); S.Push(y); C.Push(XIf); C.Push(x)
            | Loop (x,y) -> S.Push(y); S.Push(x); C.Push(XLoop); C.Push(x)
            | Seq (x,y) -> C.Push(y); C.Push(x)
            //p3
            | Blkf (x,y) -> C.Push(y); C.Push(x)
            | Dec (x,y) -> C.Push(y); C.Push(x)
            | Blk x -> C.Push(XBlk); C.Push(x)
            | Prc (x,y,z) -> this.addFunProc x (Abs(y,z))
            | Prcf (x,z) -> this.addFunProc x (Absf(z))
            | Fun (x,y,z) -> this.addFunProc x (Abs(y,z))
            | Funf (x,z) -> this.addFunProc x (Absf(z))
            | Ret x -> match x with
                | Id x -> S.Push(this.findIdValue(x))
            | Cal (x,y) -> C.Push(XCal); C.Push(y); C.Push(x)
            | Calf x -> C.Push(XCalf); C.Push(x)
            | Act x -> S.Push(Act(x))
            | XCal -> match S.Pop(), S.Pop() with
                        | Act x, Id y -> this.FunPrc (Id(y)) (Act(x))
            | XCalf -> match S.Pop() with
                        | Id x -> this.FunPrc (Id(x)) Empty
            | XBlk -> match S.Pop() with
                        | Enviroment x -> E <- x; this.garbageCollector
            | Print x -> printfn "Sheila says: %A" x
            | VarDec x -> ()
            | ConstDec x -> ()
            | Init (x,y) -> ()
            //Actions
            | XAdd -> match S.Pop(), S.Pop() with
                        | Number x, Number y -> (S.Push(Number(x + y)))
                        | Id x, Number y -> match this.findIdValue(x) with
                            | Number x -> (S.Push(Number(x + y)))
                        | Number x, Id y -> match this.findIdValue(y) with
                            | Number y -> (S.Push(Number(x + y)))
                        | Id x, Id y -> match this.findIdValue(x), this.findIdValue(y) with
                            | Number x, Number y -> (S.Push(Number(x + y)))
            | XSubtract -> match S.Pop(), S.Pop() with
                        | Number x, Number y -> (S.Push(Number(y - x)))
                        | Id x, Number y -> match this.findIdValue(x) with
                            | Number x -> (S.Push(Number(y - x)))
                        | Number x, Id y -> match this.findIdValue(y) with
                            | Number y -> (S.Push(Number(y - x)))
                        | Id x, Id y -> match this.findIdValue(x), this.findIdValue(y) with
                            | Number x, Number y -> (S.Push(Number(y - x)))
            | XMultiply -> match S.Pop(), S.Pop() with
                        | Number x, Number y -> (S.Push(Number(x * y)))
                        | Id x, Number y -> match this.findIdValue(x) with
                            | Number x -> (S.Push(Number(x * y)))
                        | Number x, Id y -> match this.findIdValue(y) with
                            | Number y -> (S.Push(Number(x * y)))
                        | Id x, Id y -> match this.findIdValue(x), this.findIdValue(y) with
                            | Number x, Number y -> (S.Push(Number(x * y)))
            | XDivide -> match S.Pop(), S.Pop() with
                        | Number x, Number y -> (S.Push(Number(y / x)))
                        | Id x, Number y -> match this.findIdValue(x) with
                            | Number x -> (S.Push(Number(y / x)))
                        | Number x, Id y -> match this.findIdValue(y) with
                            | Number y -> (S.Push(Number(y / x)))
                        | Id x, Id y -> match this.findIdValue(x), this.findIdValue(y) with
                            | Number x, Number y -> (S.Push(Number(y / x)))
            | XAnd -> match S.Pop(), S.Pop() with
                        | Boolean x, Boolean y -> (S.Push(Boolean(x && y)))
                        | Id x, Boolean y -> match this.findIdValue(x) with
                            | Boolean x -> (S.Push(Boolean(x && y)))
                        | Boolean x, Id y -> match this.findIdValue(y) with
                            | Boolean y -> (S.Push(Boolean(x && y)))
                        | Id x, Id y -> match this.findIdValue(x), this.findIdValue(y) with
                            | Boolean x, Boolean y -> (S.Push(Boolean(x && y)))
            | XOr -> match S.Pop(), S.Pop() with
                        | Boolean x, Boolean y -> (S.Push(Boolean(x || y)))
                        | Id x, Boolean y -> match this.findIdValue(x) with
                            | Boolean x -> (S.Push(Boolean(x || y)))
                        | Boolean x, Id y -> match this.findIdValue(y) with
                            | Boolean y -> (S.Push(Boolean(x || y)))
                        | Id x, Id y -> match this.findIdValue(x), this.findIdValue(y) with
                            | Boolean x, Boolean y -> (S.Push(Boolean(x || y)))
            | XNeg -> match S.Pop() with
                        | Boolean x -> (S.Push(Boolean(not(x))))
                        | Id x -> match this.findIdValue(x) with
                            | Boolean x -> (S.Push(Boolean(not(x))))
            | XEq -> match S.Pop(), S.Pop() with
                        | Boolean x, Boolean y -> (S.Push(Boolean(x = y)))
                        | Number x, Number y -> (S.Push(Boolean(x = y)))
                        | Id x, Boolean y -> match this.findIdValue(x) with
                            | Boolean x -> (S.Push(Boolean(x = y)))
                        | Boolean x, Id y -> match this.findIdValue(y) with
                            | Boolean y -> (S.Push(Boolean(x = y)))
                        | Id x, Number y -> match this.findIdValue(x) with
                            | Number x -> (S.Push(Boolean(x = y)))
                        | Number x, Id y -> match this.findIdValue(y) with
                            | Number y -> (S.Push(Boolean(x = y)))
                        | Id x, Id y -> match this.findIdValue(x), this.findIdValue(y) with
                            | Boolean x, Boolean y -> (S.Push(Boolean(x = y)))
                        | Id x, Id y -> match this.findIdValue(x), this.findIdValue(y) with
                            | Boolean x, Boolean y -> (S.Push(Boolean(x = y)))
                            | Number x, Number y -> (S.Push(Boolean(x = y)))
            | XNeq -> match S.Pop(), S.Pop() with
                        | Boolean x, Boolean y -> (S.Push(Boolean(x <> y)))
                        | Number x, Number y -> (S.Push(Boolean(x <> y)))
                        | Id x, Boolean y -> match this.findIdValue(x) with
                            | Boolean x -> (S.Push(Boolean(x <> y)))
                        | Boolean x, Id y -> match this.findIdValue(y) with
                            | Boolean y -> (S.Push(Boolean(x <> y)))
                        | Id x, Number y -> match this.findIdValue(x) with
                            | Number x -> (S.Push(Boolean(x <> y)))
                        | Number x, Id y -> match this.findIdValue(y) with
                            | Number y -> (S.Push(Boolean(x <> y)))
                        | Id x, Id y -> match this.findIdValue(x), this.findIdValue(y) with
                            | Boolean x, Boolean y -> (S.Push(Boolean(x <> y)))
                        | Id x, Id y -> match this.findIdValue(x), this.findIdValue(y) with
                            | Boolean x, Boolean y -> (S.Push(Boolean(x <> y)))
                            | Number x, Number y -> (S.Push(Boolean(x <> y)))
            | XLeb -> match S.Pop(), S.Pop() with
                        | Number x, Number y -> (S.Push(Boolean(y < x)))
                        | Id x, Number y -> match this.findIdValue(x) with
                            | Number x -> (S.Push(Boolean(y < x)))
                        | Number x, Id y -> match this.findIdValue(y) with
                            | Number y -> (S.Push(Boolean(y < x)))
                        | Id x, Id y -> match this.findIdValue(x), this.findIdValue(y) with
                            | Number x, Number y -> (S.Push(Boolean(y < x)))
            | XLeq -> match S.Pop(), S.Pop() with
                        | Number x, Number y -> (S.Push(Boolean(y <= x)))
                        | Id x, Number y -> match this.findIdValue(x) with
                            | Number x -> (S.Push(Boolean(y <= x)))
                        | Number x, Id y -> match this.findIdValue(y) with
                            | Number y -> (S.Push(Boolean(y <= x)))
                        | Id x, Id y -> match this.findIdValue(x), this.findIdValue(y) with
                            | Number x, Number y -> (S.Push(Boolean(y <= x)))
            | XGeb -> match S.Pop(), S.Pop() with
                        | Number x, Number y -> (S.Push(Boolean(y > x)))
                        | Id x, Number y -> match this.findIdValue(x) with
                            | Number x -> (S.Push(Boolean(y > x)))
                        | Number x, Id y -> match this.findIdValue(y) with
                            | Number y -> (S.Push(Boolean(y > x)))
                        | Id x, Id y -> match this.findIdValue(x), this.findIdValue(y) with
                            | Number x, Number y -> (S.Push(Boolean(y > x)))
            | XGeq -> match S.Pop(), S.Pop() with
                        | Number x, Number y -> (S.Push(Boolean(y >= x)))
                        | Id x, Number y -> match this.findIdValue(x) with
                            | Number x -> (S.Push(Boolean(y >= x)))
                        | Number x, Id y -> match this.findIdValue(y) with
                            | Number y -> (S.Push(Boolean(y >= x)))
                        | Id x, Id y -> match this.findIdValue(x), this.findIdValue(y) with
                            | Number x, Number y -> (S.Push(Boolean(y >= x)))
            | XAssign -> match S.Pop(), this.checkVarType(S.Pop()) with 
                            | Number y, Id x -> try (M.Add(E.Item(string x),Number y))
                                                with
                                                | :? System.ArgumentException -> M.Remove(E.Item(string x)) ; M.Add(E.Item(string x),Number y)
                            | Boolean y, Id x -> try (M.Add(E.Item(string x),Boolean y))
                                                    with
                                                    | :? System.ArgumentException -> M.Remove(E.Item(string x))  ; M.Add(E.Item(string x),Boolean y)
                            | Id y, Id x      -> try (M.Add(E.Item(string y),M.Item(Id(x))))
                                                    with
                                                    | :? System.ArgumentException -> M.Remove(E.Item(string x))  ; (M.Add(E.Item(string y),E.Item(string x)))
            | XIf -> match S.Pop(), S.Pop(), S.Pop() with
                        | Boolean x, y, z -> match x with
                            | true -> C.Push(y)
                            | false -> C.Push(z)
            | XLoop -> match S.Pop(), S.Pop(), S.Pop() with
                        | Boolean x, y, z -> match x with
                            | true -> C.Push(Loop(y,z)); C.Push(z)
                            | false -> ()
            | _ -> this.enviromentStuff op
            ; this.aKindOfMagic

let stackator (impBplc : Tipao) (eSMC : ESMC) =
    eSMC.fillController(impBplc)

let getFromParser (program) (eSMC : ESMC) =
    match program with
    | Success r -> printfn "Input = %A\n-----" r.value; stackator r.value eSMC
    | Failure f -> printfn "Parsing falhou! %A" f.index //failwith "Parsing falhou!"