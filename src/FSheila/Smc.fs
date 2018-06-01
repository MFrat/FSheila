﻿module FSheila.Smc

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
        let location = Location(int -1)
        for i in 0 .. M.Keys.Count do
            let idx = Location(int i)
            match (M.ContainsKey(idx)) with
            | true ->
                let location = idx
                M.Add(location, c)
            | _ -> ()
        
        if location = Location(int -1) then
            let location = Location(int M.Keys.Count)
            M.Add(location, c)
            location
        else
            location

    member private this.garbageCollector =
        let toRemove = []
        for entry in Dictionary<Tipao, Tipao>(M) do //faz uma cópia da memória para iterar
            match entry.Key with
            | Location l -> match E.ContainsValue(Location l) with
                | false -> M.Remove(Location l) |> ignore
                | _ -> ()

    member this.enviroment (op : Tipao) = 
        match op with
        | ConstBlock (x,y) -> S.Push(XEnviroment(this.newEnviroment)); match x,y with
            | ConstInit (a,b), y ->
            match b with
                | Id b -> E.Add(string(a), Id(b))
                | Number b -> E.Add(string(a), Number(b))
                | Boolean b -> E.Add(string(a), Boolean(b))
            ; C.Push(XConstBlock); C.Push(y)
        | XConstBlock -> match S.Pop() with
            | XEnviroment x -> E <- x
        | VarBlock (x,y) -> S.Push(XEnviroment(this.newEnviroment)); match x,y with
            | VarInit (a,b), y ->
            match b with
                | Id b -> E.Add(string(a), this.setOnMemory(Id(b)))
                | Number b -> E.Add(string(a), this.setOnMemory(Number(b)))
                | Boolean b -> E.Add(string(a), this.setOnMemory(Boolean(b)))
            ; C.Push(XVarBlock); C.Push(y)
        | XVarBlock -> match S.Pop() with //ALEM DISSO PRECISA LIMPAR A MEMORIA
            | XEnviroment x -> E <- x; this.garbageCollector
        | _ -> failwith "Deu ruim"

    member this.aKindOfMagic =
        if C.Count <> 0 then  
            this.print
            let op = C.Pop()
            match op with
            //Default cases
            | Number x -> S.Push(Number(x))
            | Boolean x -> S.Push(Boolean(x))
            | Id x -> S.Push(M.Item(E.Item(string x)))
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
            //Actions
            | XAdd -> match S.Pop(), S.Pop() with
                        | Number x, Number y -> (S.Push(Number(x + y)))
            | XSubtract -> match S.Pop(), S.Pop() with
                        | Number x, Number y -> (S.Push(Number(y - x)))
            | XMultiply -> match S.Pop(), S.Pop() with
                        | Number x, Number y -> (S.Push(Number(x * y)))
            | XDivide -> match S.Pop(), S.Pop() with
                        | Number x, Number y -> (S.Push(Number(y / x)))
            | XAnd -> match S.Pop(), S.Pop() with
                        | Boolean x, Boolean y -> (S.Push(Boolean(x && y)))
            | XOr -> match S.Pop(), S.Pop() with
                        | Boolean x, Boolean y -> (S.Push(Boolean(x || y)))
            | XNeg -> match S.Pop() with
                        | Boolean x -> (S.Push(Boolean(not(x))))
            | XEq -> match S.Pop(), S.Pop() with
                        | Boolean x, Boolean y -> (S.Push(Boolean(x = y)))
                        | Number x, Number y -> (S.Push(Boolean(x = y)))             
            | XNeq -> match S.Pop(), S.Pop() with
                        | Boolean x, Boolean y -> (S.Push(Boolean(x <> y)))
                        | Number x, Number y -> (S.Push(Boolean(x <> y)))                
            | XLeb -> match S.Pop(), S.Pop() with
                        | Number x, Number y -> (S.Push(Boolean(y < x)))
            | XLeq -> match S.Pop(), S.Pop() with
                        | Number x, Number y -> (S.Push(Boolean(y <= x)))
            | XGeb -> match S.Pop(), S.Pop() with
                        | Number x, Number y -> (S.Push(Boolean(y > x)))
            | XGeq -> match S.Pop(), S.Pop() with
                        | Number x, Number y -> (S.Push(Boolean(y >= x)))
            | XAssign -> match S.Pop(), S.Pop() with
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
            | _ -> this.enviroment op
            ; this.aKindOfMagic

let stackator (impBplc : Tipao) (eSMC : ESMC) =
    eSMC.fillController(impBplc)

let getFromParser (program) (eSMC : ESMC) =
    match program with
    | Success r -> printfn "Input = %A" r.value; stackator r.value eSMC
    | Failure f -> printfn "%A" f.index //failwith "Parsing falhou!"