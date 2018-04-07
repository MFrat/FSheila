// Saiba mais sobre F# em http://fsharp.org
// Veja o projeto 'F# Tutorial' para obter mais ajuda.
module teste.teste

open ScanRat
open ScanRatCombinators
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
            | Boolean of bool



type PEGParser () = 
        //vale a pena lembrar que os operadores --> vão sair; a semântica das operãções vão vir da BPLC

        //basic shit:
        member this.whitespace = (~~" ").oneOrMore
        member this.breakline = (~~"\n").oneOrMore

        member this.posNumber = oneOf "0123456789" --> fun a -> int(a) - int('0')
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
        
        member this.addOp = this.whitespace.opt + ~~"+" + this.whitespace.opt
        member this.subOp = this.whitespace.opt + ~~"-" + this.whitespace.opt
        member this.mulOp = this.whitespace.opt + ~~"*" + this.whitespace.opt
        member this.divOp = this.whitespace.opt + ~~"/" + this.whitespace.opt
        member this.choOp = this.whitespace.opt + ~~"|" + this.whitespace.opt
        //provável que as operações seja definidas conforme abaixo
        //por enquanto sem poder usar espaço entre os valores:
        member this.calcOp = 
                let multiplicative = production "multiplicative"
                let additive = production "additive"
        
                let number = this.number --> Number
                //esse number acima é para manter todo mundo do mesmo tipo (Exp). Se não usar a regra do scanrat reclama de tipos inconsistentes na mesma regra.

                let add = additive.+ ~~"+" +  multiplicative  --> Add
                let sub = additive .+ ~~"-" + multiplicative --> Subtract

                let multiply = multiplicative .+ ~~"*" + number --> Multiply
                let divide = multiplicative .+ ~~"/" + number --> Divide

                additive.rule 
                    <- add 
                    |- sub 
                    |- multiplicative

                multiplicative.rule 
                    <- multiply 
                    |- divide 
                    |- number

                additive
        //boolOp is under construction (nao testa que tá bugado)
        member this.boolOp =
             let andOp = production "andOp"
             let orOp = production "orOp"

             let boolean = this.booleanType --> Boolean
             //se não me engano and tem precedência sobre or.
             let ourAnd = andOp .+ ~~"and" + orOp --> And
             let ourOr = andOp .+ ~~"or" + orOp --> Or
             let ourNeg = ~~"~" +. boolean --> Neg

             andOp.rule
                 <- ourAnd
                 |- ourOr
                 |- ourNeg
                 |- boolean

             orOp.rule
                 <- ourAnd
                 |- ourOr
                 |- ourNeg
                 |- boolean



        //como encaixar isso aqui embaixo na ideia introduzida acima é cena pro próximo capítulo.
        member this.negOp = this.whitespace.opt + ~~"~"
        member this.eqOp = this.whitespace.opt + ~~"==" + this.whitespace.opt
        member this.lebOp = this.whitespace.opt + ~~"<" + this.whitespace.opt
        member this.leqOp = this.whitespace.opt + ~~"<=" + this.whitespace.opt
        member this.gebOp = this.whitespace.opt + ~~">" + this.whitespace.opt
        member this.geqOp = this.whitespace.opt + ~~">=" + this.whitespace.opt
        member this.orOp = this.whitespace.opt + ~~"or" + this.whitespace.opt
        member this.andOp = this.whitespace.opt + ~~"and" + this.whitespace.opt

        member this.assignOp = this.whitespace.opt + ~~":=" + this.whitespace.opt
        member this.varOp = ~~"var" + this.whitespace
        member this.consOp = ~~"cons" + this.whitespace
        member this.initOp = ~~"init" + this.whitespace
        member this.iniOp = this.whitespace.opt + ~~"=" + this.whitespace.opt

        //arithmetic expressions:

        //REVER TODOS, TÁ TUDO ERRADO
        member this.addRule = this.number + this.addOp + this.number //--> fun (a,b) -> fst(a) + b
        member this.subRule = this.number + this.subOp + this.number //--> fun (a,b) -> fst(a) - b
        member this.mulRule = this.number + this.mulOp + this.number //--> fun (a,b) -> fst(a) * b
        member this.divRule = this.number + this.divOp + this.number //--> fun (a,b) -> fst(a) / b
        member this.mathRule = this.addRule |- this.subRule |- this.mulRule |- this.divRule

        //boolean expressions:

        //REVER TODOS, TÁ TUDO ERRADO
        member this.eqRule = this.booleanType + this.eqOp + this.booleanType //--> fun (a,b) -> fst(a) = b
        member this.lebRule = this.booleanType + this.lebOp + this.booleanType
        member this.leqRule = this.booleanType + this.leqOp + this.booleanType
        member this.gebRule = this.booleanType + this.gebOp + this.booleanType
        member this.geqRule = this.booleanType + this.geqOp + this.booleanType
        member this.orRule = this.booleanType + this.orOp + this.booleanType
        member this.andRule = this.booleanType + this.andOp + this.booleanType
        member this.boolRule = this.eqRule |- this.lebRule |- this.leqRule |- this.gebRule |- this.geqRule |- this.orRule |- this.andRule
        member this.negRule = this.negOp + this.boolRule  //DUVIDA AQUI TEM QUE REVER NEG: o tipo de negRUle é diferente das demais por ter uma aridade diferente.

        //general expressions:
        //trocando o number por booleantype em regras booleanas, a regra abaixo passa a não valer:
        //member this.expRule = (this.mathRule |- this.boolRule)

       
        //commands:

        //member this.assignRule = this.id + this.assignOp + this.expRule
        member this.varRule = this.varOp + this.id
        //member this.atribRule = this.varRule + this.assignRule //var num := 2 + 2
        member this.consRule = this.consOp + this.id
        //member this.iniRule = this.id + this.iniOp + this.expRule
        //member this.initRule = this.initOp + this.iniRule

[<EntryPoint>]
let main argv = 
    //if 3 = 3 then printfn "a"
    let testGrammar = new PEGParser()
    let teste = parse testGrammar.calcOp "2+2+3+4+5"
    let teste2 = parse testGrammar.boolOp "true and false or false and true"
    printfn "%A" teste
    let sheila = Console.ReadLine()
    printfn "%A" sheila
    //printfn "%A" argv
    0 // retornar um código de saída inteiro