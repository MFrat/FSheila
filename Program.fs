// Saiba mais sobre F# em http://fsharp.org
// Veja o projeto 'F# Tutorial' para obter mais ajuda.
module teste.teste

open ScanRat
open System
//tipo base de operações booleanas
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
 //tipo base de expressões matemáticas
type Exp =  
            | Add of Exp * Exp
            | Subtract of Exp * Exp
            | Multiply of Exp * Exp
            | Divide of Exp * Exp
            | Number of int
            | Boolexp of boolExp
//tipo base de operações de comando
type Cmd =
         //id é apenas uma string que representa o nome da variável
         | Var of string
         | Assign of string * Exp
         | If of boolExp * Cmd
         | Loop of boolExp * Cmd
         | Seq of boolExp * Cmd
         


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
        
        member this.id = (this.letter |- this.digit).oneOrMore --> fun l -> System.String.Concat(l)

        //operators:
        //regras de parsing de operações numéricas
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
        //regras de parsing de comparações numéricas
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
        //a ideia é fazer algo parecido com o que foi feito nas duas situações acima
        //as 5 operações abaixo estão incorretas e serão removidas
        //member this.assignOp = this.whitespace.oneOrMore.opt + ~~":=" + this.whitespace.oneOrMore.opt
        member this.varOp = ~~"var" //+ this.whitespace.oneOrMore.opt
        member this.consOp = ~~"cons" + this.whitespace.oneOrMore.opt
        member this.initOp = ~~"init" + this.whitespace.oneOrMore.opt
        member this.iniOp = this.whitespace.oneOrMore.opt + ~~"=" + this.whitespace.oneOrMore.opt


        //commands:
        //funciona, mas não sei se é o ideal: abaixo a regra de parsing de var <ident>^+
        member this.varRule = 
               let var = production "var"
               //segundo a regra abaixo eu forço ter pelo menos um espaço depoi da keyword "var"
               let oneVar = ~~"var" + (this.whitespace.oneOrMore +. this.id .+ this.whitespace.oneOrMore.opt)
               let moreVars =  ~~"," +. (this.whitespace.oneOrMore +. this.id .+ this.whitespace.oneOrMore.opt) //.+ ~~","+. (this.whitespace.oneOrMore.opt +. this.id .+ this.whitespace.oneOrMore.opt)
               var.rule
                  <- oneVar + moreVars.oneOrMore.opt
               var

        member this.constRule = 
               let constAtr = production "const"
               //segundo a regra abaixo eu forço ter pelo menos um espaço depois da keyword "const"
               let oneConst = ~~"const" + (this.whitespace.oneOrMore +. this.id .+ this.whitespace.oneOrMore.opt)
               let moreConsts =  ~~"," +. (this.whitespace.oneOrMore +. this.id .+ this.whitespace.oneOrMore.opt) //.+ ~~","+. (this.whitespace.oneOrMore.opt +. this.id .+ this.whitespace.oneOrMore.opt)
               constAtr.rule
                  <- oneConst + moreConsts.oneOrMore.opt
               constAtr

        //regra do assign
        //nota: por enquanto só funciona para atribuições numéricas (inclusive atribuição de expressões numéricas). Falta eu fazer rodar pra operações booleanas tbm.
        member this.assignRule =
             //let boole = this.boolOp
             let numExp = this.calcOp //|- boole
             //let boolEx = this.boolOp
             let assignRule = production "assignRule"
             let oneAssignExp = (this.whitespace.oneOrMore +. this.id .+ this.whitespace.oneOrMore.opt) .+ ~~"=" + (this.whitespace.oneOrMore +. numExp .+ this.whitespace.oneOrMore.opt) --> Assign
             //let oneConstBoolExp =  ~~"=" + (this.whitespace.oneOrMore + Boolexp .+ this.whitespace.oneOrMore.opt) --> Assign
             //NOTA: para o uso de múltiplos assigns é necessário ter um espaço como definido no this.whitespace.oneOrMore (vide documentação de IMP).
             let moreAssigns =  ~~"," +. (this.whitespace.oneOrMore +. this.id .+ this.whitespace.oneOrMore.opt) .+  ~~"=" + (this.whitespace.oneOrMore +. numExp .+ this.whitespace.oneOrMore.opt) --> Assign //.+ ~~","+. (this.whitespace.oneOrMore.opt +. this.id .+ this.whitespace.oneOrMore.opt)
             assignRule.rule
                //o init não precisa ser levado como dado importante para o processo de semântica pela definição da regra acima (note que o mesmo ocorre com "var" e "const").
                <- ~~"init" +. (oneAssignExp + moreAssigns.oneOrMore.opt)
             assignRule

        //member this.assignRule = this.id + this.assignOp + this.expRule
        //member this.varRule = this.varOp +. this.id
        //member this.atribRule = this.varRule + this.assignRule //var num := 2 + 2
        //member this.consRule = this.consOp + this.id
        //member this.iniRule = this.id + this.iniOp + this.expRule
        //member this.initRule = this.initOp + this.iniRule


[<EntryPoint>]
let main argv = 
    let testGrammar = new PEGParser()
    //let teste = parse testGrammar.calcOp "-21 + 5555 + 3 + 4 * 666 /   5"
    //let teste = parse testGrammar.boolOp "true and 3==3 and true and false";
    //let teste2 = parse testGrammar.boolOp "true and false or false and true"
    //let teste = parse testGrammar.boolOp "3<=4 and 4<>5 or true and false and 666  <= 4 or 1981> 2007"
    //let teste = parse testGrammar.varRule "var x , y , z, a"
    //let teste = parse testGrammar.constRule "const abc , x , y , a69"
    let teste = parse testGrammar.assignRule "init x = 2 , y = 555*6 , abhe =   1981"
    //printfn "%A" teste2
    printfn "%A" teste
    let sheila = Console.ReadLine()
    printfn "%A" sheila
    //printfn "%A" argv
    0 // retornar um código de saída inteiro