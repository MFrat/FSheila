// Saiba mais sobre F# em http://fsharp.org
// Veja o projeto 'F# Tutorial' para obter mais ajuda.
module teste.teste

open ScanRat
open ScanRatCombinators
open System



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
        
        member this.number = this.posNumber |- this.negNumber
        member this.letter = this.lLetter |- this.uLetter
        
        member this.id = (this.letter |- this.digit).oneOrMore

        //operators:
        
        member this.addOp = this.whitespace.opt + ~~"+" + this.whitespace.opt
        member this.subOp = this.whitespace.opt + ~~"-" + this.whitespace.opt
        member this.mulOp = this.whitespace.opt + ~~"*" + this.whitespace.opt
        member this.divOp = this.whitespace.opt + ~~"/" + this.whitespace.opt
        member this.choOp = this.whitespace.opt + ~~"|" + this.whitespace.opt
        
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
        member this.eqRule = this.number + this.eqOp + this.number //--> fun (a,b) -> fst(a) = b
        member this.lebRule = this.number + this.lebOp + this.number
        member this.leqRule = this.number + this.leqOp + this.number
        member this.gebRule = this.number + this.gebOp + this.number
        member this.geqRule = this.number + this.geqOp + this.number
        member this.orRule = this.number + this.orOp + this.number
        member this.andRule = this.number + this.andOp + this.number
        member this.boolRule = this.eqRule |- this.lebRule |- this.leqRule |- this.gebRule |- this.geqRule |- this.orRule |- this.andRule
        member this.negRule = this.negOp + this.boolRule  //DUVIDA AQUI TEM QUE REVER NEG

        //general expressions:

        member this.expRule = (this.mathRule |- this.boolRule)

        //o retorno de ambos os parsings são iguais. até aqui as regras não possuem semântica, então está ok.
        //commands:

        member this.assignRule = this.id + this.assignOp + this.expRule
        member this.varRule = this.varOp + this.id
        member this.consRule = this.consOp + this.id
        member this.iniRule = this.id + this.iniOp + this.expRule
        member this.initRule = this.initOp + this.iniRule

[<EntryPoint>]
let main argv = 
    //if 3 = 3 then printfn "a"
    let testGrammar = new PEGParser()
    let teste = parse testGrammar.addRule "1+2"
    printfn "%A" teste
    let sheila = Console.ReadLine()
    printfn "%A" sheila
    //printfn "%A" argv
    0 // retornar um código de saída inteiro