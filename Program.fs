// Saiba mais sobre F# em http://fsharp.org
// Veja o projeto 'F# Tutorial' para obter mais ajuda.
module teste.teste

open ScanRat
open System

type Exp =  
            | Add of Exp * Exp
            | Subtract of Exp * Exp
            | Multiply of Exp * Exp
            | Divide of Exp * Exp
            | Number of int

type boolExp = 
            | And of boolExp * boolExp
            | Or of boolExp * boolExp
            | Neg of boolExp
            | Eq of int * int 
            | Leb of int * int
            | Leq of int * int
            | Geb of int * int
            | Geq of int * int
            | Neq of int * int
            | Boolean of bool



type PEGParser () = 
        //vale a pena lembrar que os operadores --> vão sair; a semântica das operãções vão vir da BPLC

        //basic shit:
        member this.whitespace = (~~" ").oneOrMore
        member this.breakline = (~~"\n").oneOrMore

        member this.posNumber =  (oneOf "0123456789").oneOrMore --> fun l -> System.String.Concat(l) |> int
        member this.negNumber = ~~"-" + this.posNumber --> fun a -> -snd(a)

        member this.digit = oneOf "0123456789"
        member this.lLetter = oneOf "abcdefghijklmnopqrstuvwxyz" --> fun a -> a
        member this.uLetter = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ" --> fun a -> a
        member this.boolTrue = ~~"true" --> fun(a) -> true
        member this.boolFalse = ~~"false" --> fun(a) -> false
        member this.booleanType = (this.boolTrue |- this.boolFalse)

        
        member this.number = this.posNumber |- this.negNumber
        member this.letter = this.lLetter |- this.uLetter
        
        member this.id = (this.letter |- this.digit).oneOrMore

        //operators:
        
        //member this.addOp = this.whitespace.opt + ~~"+" + this.whitespace.opt
        //member this.subOp = this.whitespace.opt + ~~"-" + this.whitespace.opt
        //member this.mulOp = this.whitespace.opt + ~~"*" + this.whitespace.opt
        //member this.divOp = this.whitespace.opt + ~~"/" + this.whitespace.opt
        //member this.choOp = this.whitespace.opt + ~~"|" + this.whitespace.opt
        // as operações serão definidas conforme abaixo
        //por enquanto sem poder usar espaço entre os valores:
        member this.calcOp = 
                let multiplicative = production "multiplicative"
                let additive = production "additive"
        
                let number = this.number --> Number
                //esse number acima é para manter todo mundo do mesmo tipo (Exp). Se não usar a regra do scanrat reclama de tipos inconsistentes na mesma regra.

                let add = (this.whitespace.oneOrMore.opt +. additive .+ this.whitespace.oneOrMore.opt) .+ ~~"+" + (this.whitespace.oneOrMore.opt +. multiplicative .+ this.whitespace.oneOrMore.opt)  --> Add
                let sub = (this.whitespace.oneOrMore.opt +. additive .+ this.whitespace.oneOrMore.opt) .+ ~~"-" + (this.whitespace.oneOrMore.opt +. multiplicative .+ this.whitespace.oneOrMore.opt) --> Subtract

                let multiply = (this.whitespace.oneOrMore.opt +. multiplicative .+ this.whitespace.oneOrMore.opt) .+ ~~"*" + (this.whitespace.oneOrMore.opt +. number .+ this.whitespace.oneOrMore.opt) --> Multiply
                let divide = (this.whitespace.oneOrMore.opt +. multiplicative .+ this.whitespace.oneOrMore.opt) .+ ~~"/" + (this.whitespace.oneOrMore.opt +. number .+ this.whitespace.oneOrMore.opt) --> Divide

                additive.rule 
                    <- add 
                    |- sub 
                    |- multiplicative

                multiplicative.rule 
                    <- multiply 
                    |- divide 
                    |- number

                additive

        //como encaixar isso aqui embaixo na ideia introduzida acima é cena pro próximo capítulo. FEITO
        //parei aqui
        member this.eqOp = (this.whitespace.oneOrMore.opt +. this.number .+ this.whitespace.oneOrMore.opt) + ~~"==" + (this.whitespace.oneOrMore.opt +. this.number .+ this.whitespace.oneOrMore.opt) --> fun (a,b) -> Eq (fst(a),b)
        member this.lebOp = (this.whitespace.oneOrMore.opt +. this.number .+ this.whitespace.oneOrMore.opt) + ~~"<" + (this.whitespace.oneOrMore.opt +. this.number .+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> Leb (fst(a),b)
        member this.leqOp = (this.whitespace.oneOrMore.opt +. this.number .+ this.whitespace.oneOrMore.opt) + ~~"<=" + (this.whitespace.oneOrMore.opt +. this.number .+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> Leq (fst(a),b)
        member this.gebOp = (this.whitespace.oneOrMore.opt +. this.number .+ this.whitespace.oneOrMore.opt) + ~~">" + (this.whitespace.oneOrMore.opt +. this.number .+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> Geb (fst(a),b)
        member this.geqOp = (this.whitespace.oneOrMore.opt +. this.number .+ this.whitespace.oneOrMore.opt) + ~~"=>" + (this.whitespace.oneOrMore.opt +. this.number .+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> Geq (fst(a),b)
        member this.neqOp = (this.whitespace.oneOrMore.opt +. this.number .+ this.whitespace.oneOrMore.opt) + ~~"<>" + (this.whitespace.oneOrMore.opt +. this.number .+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> Neq (fst(a),b)
        //member this.orOp = this.whitespace.oneOrMore.opt + ~~"or" + this.whitespace.oneOrMore.opt
        //member this.andOp = this.whitespace.oneOrMore.opt + ~~"and" + this.whitespace.oneOrMore.opt

        member this.boolOp =
             let andOp = production "andOp"
             let orOp = production "orOp"

             let boolean = this.booleanType --> Boolean
             //se não me engano and tem precedência sobre or.
             let ourAnd = (this.whitespace.oneOrMore.opt +. andOp .+ this.whitespace.oneOrMore.opt) .+ ~~"and" + (this.whitespace.oneOrMore.opt +. orOp .+ this.whitespace.oneOrMore.opt) --> And
             let ourOr = (this.whitespace.oneOrMore.opt +. andOp .+ this.whitespace.oneOrMore.opt) .+ ~~"or" + (this.whitespace.oneOrMore.opt +. orOp .+ this.whitespace.oneOrMore.opt) --> Or
             let ourNeg = ~~"~" +. boolean --> Neg

             andOp.rule
                 <- ourAnd
                 |- ourOr
                 |- ourNeg
                 |- this.eqOp
                 |- this.lebOp
                 |- this.leqOp
                 |- this.gebOp
                 |- this.geqOp
                 |- this.neqOp
                 |- boolean

             orOp.rule
                 <- ourAnd
                 |- ourOr
                 |- ourNeg
                 |- this.eqOp
                 |- this.lebOp
                 |- this.leqOp
                 |- this.gebOp
                 |- this.geqOp
                 |- this.neqOp
                 |- boolean
                
             andOp

        member this.assignOp = this.whitespace.oneOrMore.opt + ~~":=" + this.whitespace.oneOrMore.opt
        member this.varOp = ~~"var" + this.whitespace
        member this.consOp = ~~"cons" + this.whitespace
        member this.initOp = ~~"init" + this.whitespace
        member this.iniOp = this.whitespace.oneOrMore.opt + ~~"=" + this.whitespace.oneOrMore.opt


        //commands:

        //member this.assignRule = this.id + this.assignOp + this.expRule
        member this.varRule = this.varOp + this.id
        //member this.atribRule = this.varRule + this.assignRule //var num := 2 + 2
        member this.consRule = this.consOp + this.id
        //member this.iniRule = this.id + this.iniOp + this.expRule
        //member this.initRule = this.initOp + this.iniRule

[<EntryPoint>]
let main argv = 
    let testGrammar = new PEGParser()
    //let teste = parse testGrammar.calcOp "-21 + 5555 + 3 + 4 * 666 /   5"
    //let teste = parse testGrammar.boolOp "true and 3==3 and true and false";
    //let teste2 = parse testGrammar.boolOp "true and false or false and true"
    let teste = parse testGrammar.boolOp "3<=4 and 4<>5 or true and false and 666  <= 4 or 1981> 2007"
    //printfn "%A" teste2
    printfn "%A" teste
    let sheila = Console.ReadLine()
    printfn "%A" sheila
    //printfn "%A" argv
    0 // retornar um código de saída inteiro