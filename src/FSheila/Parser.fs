// Saiba mais sobre F# em http://fsharp.org
// Veja o projeto 'F# Tutorial' para obter mais ajuda.
module FSheila.Parser

open ScanRat

//tipo base de operações booleanas
//type boolExp = 
//            | And of boolExp * boolExp
//            | Or of boolExp * boolExp
//            | Neg of boolExp
//            | Eq of int * int 
//            | Leb of int * int
//            | Leq of int * int
//            | Geb of int * int
//            | Geq of int * int
//            | Neq of int * int
//            | Boolean of bool
 //tipo base de expressões matemáticas

//type Exp =  
//            | Add of Exp * Exp
//            | Subtract of Exp * Exp
//            | Multiply of Exp * Exp
//            | Divide of Exp * Exp
//            | And of Exp * Exp
//            | Or of Exp * Exp
//            | Neg of Exp
//            | Eq of  Exp * Exp
//            | Leb of Exp * Exp
//            | Leq of Exp * Exp
//            | Geb of Exp * Exp
//            | Geq of Exp * Exp
//            | Neq of Exp * Exp
//            | Number of int
//            | Boolean of bool
//            | Id of string
//tipo base de operações de comando
type Cmd =
         //id é apenas uma string que representa o nome da variável
         | Var of string
         | Assign of string * Cmd
         | Init of string * Cmd
         | If of Cmd * Cmd * Cmd //boolCmd vira Cmd
         | Loop of Cmd * Cmd // Cmd list (ou não -->) //um bloco é visto pelo ScanRat como uma lista de comandos.
         | Seq of Cmd * Cmd
         | Add of Cmd * Cmd
         | Subtract of Cmd * Cmd
         | Multiply of Cmd * Cmd
         | Divide of Cmd * Cmd
         | And of Cmd * Cmd
         | Or of Cmd * Cmd
         | Neg of Cmd
         | Eq of  Cmd * Cmd
         | Leb of Cmd * Cmd
         | Leq of Cmd * Cmd
         | Geb of Cmd * Cmd
         | Geq of Cmd * Cmd
         | Neq of Cmd * Cmd
         | Number of int
         | Boolean of bool
         | Id of string

         //| Block of Cmd //added p/ tentar fazer o if funcionar com o uso de blocos de comando.

type PEGParser () = 
        //vale a pena lembrar que os operadores --> vão sair; a semântica das operãções vão vir da BPLC

        //basic shit:
        member this.whitespace = (~~" ").oneOrMore
        member this.linebreak = (~~"\r\n").oneOrMore

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
        
        //correção no id: antes permitia apenas um número ser um identificador.
        member this.id = (this.letter + (this.letter |- this.digit).oneOrMore) --> fun (a,l) -> a::l |> System.String.Concat 

        //operators:
        //regras de parsing de operações numéricas
        member this.calcOp = 
                let multiplicative = production "multiplicative"
                let additive = production "additive"
        
                let number = this.number --> Number
                let id = this.id --> Id
                //esse number acima é para manter todo mundo do mesmo tipo (Exp). Se não usar a regra do scanrat reclama de tipos inconsistentes na mesma regra.

                let add = (this.whitespace.oneOrMore.opt +. additive .+ this.whitespace.oneOrMore.opt) .+ ~~"+" + (this.whitespace.oneOrMore.opt +. multiplicative .+ this.whitespace.oneOrMore.opt)  --> Add
                let sub = (this.whitespace.oneOrMore.opt +. additive .+ this.whitespace.oneOrMore.opt) .+ ~~"-" + (this.whitespace.oneOrMore.opt +. multiplicative .+ this.whitespace.oneOrMore.opt) --> Subtract

                let multiply = (this.whitespace.oneOrMore.opt +. multiplicative .+ this.whitespace.oneOrMore.opt) .+ ~~"*" + (this.whitespace.oneOrMore.opt +. (number |- id) .+ this.whitespace.oneOrMore.opt) --> Multiply
                let divide = (this.whitespace.oneOrMore.opt +. multiplicative .+ this.whitespace.oneOrMore.opt) .+ ~~"/" + (this.whitespace.oneOrMore.opt +. (number |- id) .+ this.whitespace.oneOrMore.opt) --> Divide

                additive.rule 
                    <- add 
                    |- sub 
                    |- multiplicative

                multiplicative.rule 
                    <- multiply 
                    |- divide 
                    |- number
                    |- id
                    

                additive
        //regras de parsing de comparações numéricas
        //member this.eqOp = (this.whitespace.oneOrMore.opt +. this.number .+ this.whitespace.oneOrMore.opt) + ~~"==" + (this.whitespace.oneOrMore.opt +. this.number .+ this.whitespace.oneOrMore.opt) --> fun (a,b) -> Eq (fst(a),b)
        //member this.lebOp = (this.whitespace.oneOrMore.opt +. this.number .+ this.whitespace.oneOrMore.opt) + ~~"<" + (this.whitespace.oneOrMore.opt +. this.number .+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> Leb (fst(a),b)
        //member this.leqOp = (this.whitespace.oneOrMore.opt +. this.number .+ this.whitespace.oneOrMore.opt) + ~~"<=" + (this.whitespace.oneOrMore.opt +. this.number .+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> Leq (fst(a),b)
        //member this.gebOp = (this.whitespace.oneOrMore.opt +. this.number .+ this.whitespace.oneOrMore.opt) + ~~">" + (this.whitespace.oneOrMore.opt +. this.number .+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> Geb (fst(a),b)
        //member this.geqOp = (this.whitespace.oneOrMore.opt +. this.number .+ this.whitespace.oneOrMore.opt) + ~~"=>" + (this.whitespace.oneOrMore.opt +. this.number .+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> Geq (fst(a),b)
        //member this.neqOp = (this.whitespace.oneOrMore.opt +. this.number .+ this.whitespace.oneOrMore.opt) + ~~"<>" + (this.whitespace.oneOrMore.opt +. this.number .+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> Neq (fst(a),b)
        //member this.compareOp = this.eqOp |- this.lebOp |- this.leqOp |-this.gebOp |- this.neqOp
        //member this.orOp = this.whitespace.oneOrMore.opt + ~~"or" + this.whitespace.oneOrMore.opt
        //member this.andOp = this.whitespace.oneOrMore.opt + ~~"and" + this.whitespace.oneOrMore.opt

        member this.boolOp =
             let andOp = production "andOp"
             let orOp = production "orOp"

             let boolean = this.whitespace.oneOrMore.opt +. this.booleanType --> Boolean
             let number = this.number --> Number
             let id = this.id --> Id
             //se não me engano and tem precedência sobre or.
             let ourAnd = (this.whitespace.oneOrMore.opt +. andOp .+ this.whitespace.oneOrMore.opt) .+ ~~"and" + (this.whitespace.oneOrMore.opt +. orOp .+ this.whitespace.oneOrMore.opt) --> And
             let ourOr = (this.whitespace.oneOrMore.opt +. andOp .+ this.whitespace.oneOrMore.opt) .+ ~~"or" + (this.whitespace.oneOrMore.opt +. orOp .+ this.whitespace.oneOrMore.opt) --> Or
             let ourNeg = this.whitespace.oneOrMore.opt + ~~"~" +. (boolean |- id) --> Neg
                        |- ( this.whitespace.oneOrMore.opt + ~~"~" + this.whitespace.oneOrMore.opt + this.whitespace.oneOrMore.opt + ~~"(" +. (andOp |- orOp) .+ this.whitespace.oneOrMore.opt .+ ~~")")  --> Neg

             let eqOp = (this.whitespace.oneOrMore.opt +. ( number |- id) .+ this.whitespace.oneOrMore.opt) + ~~"==" + (this.whitespace.oneOrMore.opt +. ( number |- id) .+ this.whitespace.oneOrMore.opt) --> fun (a,b) -> Eq (fst(a),b)
             let lebOp = (this.whitespace.oneOrMore.opt +. ( number |- id) .+ this.whitespace.oneOrMore.opt) + ~~"<" + (this.whitespace.oneOrMore.opt +. ( number |- id) .+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> Leb (fst(a),b)
             let leqOp = (this.whitespace.oneOrMore.opt +. ( number |- id) .+ this.whitespace.oneOrMore.opt) + ~~"<=" + (this.whitespace.oneOrMore.opt +. ( number |- id) .+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> Leq (fst(a),b)
             let gebOp = (this.whitespace.oneOrMore.opt +. ( number |- id) .+ this.whitespace.oneOrMore.opt) + ~~">" + (this.whitespace.oneOrMore.opt +. ( number |- id) .+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> Geb (fst(a),b)
             let geqOp = (this.whitespace.oneOrMore.opt +. ( number |- id) .+ this.whitespace.oneOrMore.opt) + ~~"=>" + (this.whitespace.oneOrMore.opt +. ( number |- id) .+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> Geq (fst(a),b)
             let neqOp = (this.whitespace.oneOrMore.opt +. ( number |- id) .+ this.whitespace.oneOrMore.opt) + ~~"<>" + (this.whitespace.oneOrMore.opt +. ( number |- id) .+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> Neq (fst(a),b)
             let compareOp = eqOp |- lebOp |- leqOp |- gebOp |- neqOp

             andOp.rule
                 <- ourAnd 
                 |- ourOr
                 |- ourNeg
                 |- compareOp
                 |- boolean
                 |- this.id --> Id

             orOp.rule
                 <- ourAnd
                 |- ourOr
                 |- ourNeg
                 |- compareOp
                 |- boolean
                 |- this.id --> Id
                
             andOp


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
        //TODO debugar essa porra aqui
        member this.initRule =
                //boolexp tá bugado
             let boole = this.boolOp //--> Boolexp
             let numExp = this.calcOp //|- boole
             //let boolEx = 
             let initRule = production "initRule"
             let oneAssignExp = ~~"init" +. (this.whitespace.oneOrMore +. this.id .+ this.whitespace.oneOrMore.opt) .+ ~~"=" + (this.whitespace.oneOrMore +. (numExp) .+ this.whitespace.oneOrMore.opt) --> Init
             let oneAssignBoolex = ~~"init" +. (this.whitespace.oneOrMore +. this.id .+ this.whitespace.oneOrMore.opt) .+ ~~"=" + (this.whitespace.oneOrMore +. (boole) .+ this.whitespace.oneOrMore.opt) --> Init
             //NOTA: para o uso de múltiplos assigns é necessário ter um espaço como definido no this.whitespace.oneOrMore (vide documentação de IMP).
             let moreAssigns = (this.whitespace.oneOrMore) + ~~"," +. (this.whitespace.oneOrMore +. this.id .+ this.whitespace.oneOrMore.opt) .+  ~~"=" + (this.whitespace.oneOrMore +. (numExp) .+ this.whitespace.oneOrMore.opt) --> Init //.+ ~~","+. (this.whitespace.oneOrMore.opt +. this.id .+ this.whitespace.oneOrMore.opt)
             let moreAssignsBool = (this.whitespace.oneOrMore) + ~~"," +. (this.whitespace.oneOrMore +. this.id .+ this.whitespace.oneOrMore.opt) .+  ~~"=" + (this.whitespace.oneOrMore +.  (boole) .+ this.whitespace.oneOrMore.opt) --> Init
             initRule.rule //tem que ver isso aqui
                //o init não precisa ser levado como dado importante para o processo de semântica pela definição da regra acima (note que o mesmo ocorre com "var" e "const").
                <- (oneAssignExp + (moreAssignsBool |- moreAssigns).opt) |- (oneAssignExp + (moreAssignsBool |- moreAssigns).opt)
             initRule
         

        //regra do assign
        //nota: por enquanto só funciona para atribuições numéricas (inclusive atribuição de expressões numéricas). Falta eu fazer rodar pra operações booleanas tbm.
        member this.assignRule =
             let boole = this.boolOp 
             let numExp = this.calcOp //|- boole
             //let boolEx = this.boolOp
             let assignRule = production "assignRule"
             let oneAssignExp = (this.whitespace.oneOrMore.opt +. this.id .+ this.whitespace.oneOrMore.opt) .+ ~~":=" + (this.whitespace.oneOrMore +. numExp .+ this.whitespace.oneOrMore.opt) --> Assign
             let oneAssignBool = (this.whitespace.oneOrMore.opt +. this.id .+ this.whitespace.oneOrMore.opt) .+ ~~":=" + (this.whitespace.oneOrMore +. boole .+ this.whitespace.oneOrMore.opt) --> Assign
             //let oneConstBoolExp =  ~~"=" + (this.whitespace.oneOrMore + Boolexp .+ this.whitespace.oneOrMore.opt) --> Assign
             //NOTA: para o uso de múltiplos assigns é necessário ter um espaço como definido no this.whitespace.oneOrMore (vide documentação de IMP).
             //NOTA2: assigns são únicos (diferentes de init). Múltiplos assigns devem fazer uso do comando de sequência.
             //let moreAssigns =  ~~";" +. (this.whitespace.oneOrMore +. this.id .+ this.whitespace.oneOrMore.opt) .+  ~~":=" + (this.whitespace.oneOrMore +. numExp .+ this.whitespace.oneOrMore.opt) --> Assign //.+ ~~","+. (this.whitespace.oneOrMore.opt +. this.id .+ this.whitespace.oneOrMore.opt)
             assignRule.rule
                //o init não precisa ser levado como dado importante para o processo de semântica pela definição da regra acima (note que o mesmo ocorre com "var" e "const",acho eu).
                <- ( oneAssignBool |- oneAssignExp) //+ moreAssigns.oneOrMore.opt)
             assignRule
         

        member this.ifRule =
               //let command = this.assignRule
               let boolExp = this.boolOp
               let block = this.blockRule

               let ifRule = production "ifRule"
               let aIf = ~~"if" +. boolExp + this.command .+ ~~"else" + this.command --> fun a -> If (fst(fst(a)),snd(fst(a)),snd(a))
               let aIfBlock =  ~~"if" +. boolExp + block .+ ~~"else" + this.command  -->  fun a -> If (fst(fst(a)),snd(fst(a)),snd(a))
               let aIfBlock2 =  ~~"if" +. boolExp + this.command .+ ~~"else" + block --> fun a -> If (fst(fst(a)),snd(fst(a)),snd(a))
               let aIfBlock3 = ~~"if" +. boolExp + block .+ ~~"else" + block -->  fun a -> If (fst(fst(a)),snd(fst(a)),snd(a))
               ifRule.rule
                     <- (aIf |- aIfBlock |- aIfBlock2 |- aIfBlock3)
               ifRule
        //regra de sequência (aparentemente 100%)
        //NOTA: na semântica de seq manter a ordem de execução de forma consistente (não sair empilhando descaradamente para depois resolver)
        member this.seqRule = //(this.whitespace.oneOrMore.opt +. this.assignRule .+ this.whitespace.oneOrMore.opt) .+ ~~";" + (this.whitespace.oneOrMore.opt +. this.assignRule .+ this.whitespace.oneOrMore.opt).oneOrMore --> fun(a,b) -> Seq (a,b)
               let command =  this.assignRule
               let seqRule = production "seqRule"
               let seq = ((this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt) +. command .+ this.whitespace.oneOrMore.opt) .+ ~~";" + ((this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt)  +. seqRule .+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> Seq (a,b)
               seqRule.rule
                  <- seq |- command  
               seqRule

        //regra do bloco de comando, parece estar 100%
        member this.blockRule = 
                let blockRule = production "blockRule"
                //let simpleCommand =  (this.whitespace.oneOrMore.opt |- this.linebreak.oneOrMore.opt)  +. (this.assignRule |- this.seqRule) .+ (this.whitespace.oneOrMore.opt |- this.linebreak.oneOrMore.opt)
                blockRule.rule
                    //gambiarra para contornar espaços depois do {
                    <- (this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + ~~"{" + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt) +. this.seqRule .+ ( this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + ~~"}" + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt) //--> Block
                blockRule
        member this.command = this.blockRule |- this.assignRule //|- this.assignRule //|- this.loopRule.oneOrMore

        //de loop só tem o while na documentação da IMP:
        member this.loopRule = (this.whitespace.oneOrMore.opt + ~~"while" + this.whitespace.oneOrMore) +. this.boolOp + this.command  --> Loop
