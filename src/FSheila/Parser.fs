module FSheila.Parser

open ScanRat
open System.Collections.Generic

type Tipao =
         //Basic typos
         | Number of bigint
         | Boolean of bool
         | Id of string
         | Location of int
         //Commands
         | Add of Tipao * Tipao
         | Subtract of Tipao * Tipao
         | Multiply of Tipao * Tipao
         | Divide of Tipao * Tipao
         | And of Tipao * Tipao
         | Or of Tipao * Tipao
         | Neg of Tipao
         | Eq of  Tipao * Tipao
         | Leb of Tipao * Tipao
         | Leq of Tipao * Tipao
         | Geb of Tipao * Tipao
         | Geq of Tipao * Tipao
         | Neq of Tipao * Tipao
         | If of Tipao * Tipao * Tipao //boolTipao vira Tipao
         | Loop of Tipao * Tipao // Tipao list (ou não -->) //um bloco é visto pelo ScanRat como uma lista de comandos.
         | Seq of Tipao * Tipao
         //Commands' orders
         | XAdd
         | XSubtract
         | XDivide 
         | XMultiply 
         | XAnd  
         | XOr  
         | XNeg  
         | XEq  
         | XLeb  
         | XLeq 
         | XGeb 
         | XGeq 
         | XNeq
         | XIf
         | XLoop
         // (novos tipos para a P2.)
         | Assign of string * Tipao
         | XAssign
         //Declarations
         //| DclConsts of Tipao
         //| DclVars of Tipao //seqûência de vars na mesma linha.
         | ConstInit of string * Tipao
         | VarInit of string * Tipao
         | Init of string * Tipao
         | SeqDec of Tipao * Tipao //sequência de declarações. Uma regra que tem um var seguido de const, ex. Pra isso, uma regra correspondente tem que ser criada no parser
         | Block of Tipao    
         | VarBlock of Tipao * Tipao //bloco declarado por declaração de var
         | ConstBlock of Tipao * Tipao //bloco declarado por declaração de const.
         | Empty
         //Declarations' orders
         | XVar //Talvez precisemos de dois tipos distintos para var e const por possuírem uma semântica diferente.
         | XConst
         | XInit
         //novos tipos p controle de blocos
         | XVarBlock
         | XConstBlock
         | XBlock
         //| Empty //usado p/ quanado há somente um bloco (no longer needed)
         | Enviroment of Dictionary<string, Tipao> //para desempilhar
         

type PEGParser () = 
        //vale a pena lembrar que os operadores --> vão sair; a semântica das operãções vão vir da BPLC

        member this.whitespace = (~~" ")
        member this.linebreak = (~~"\r\n").oneOrMore

        member this.posNumber =  (oneOf "0123456789").oneOrMore --> fun l -> System.String.Concat(l) |> int |> bigint
        member this.negNumber = this.whitespace.oneOrMore.opt +. ~~"(" + ~~"-" +. this.posNumber .+ ~~")" .+ this.whitespace.oneOrMore.opt --> fun a -> -a
                                |- this.whitespace.oneOrMore.opt +. ~~"-" +. this.posNumber .+ this.whitespace.oneOrMore.opt--> fun a -> -a

        member this.digit = oneOf "0123456789"
        member this.lLetter = oneOf "abcdefghijklmnopqrstuvwxyz" --> fun a -> a
        member this.uLetter = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ" --> fun a -> a
        member this.boolTrue = ~~"true" --> fun(a) -> true
        member this.boolFalse = ~~"false" --> fun(a) -> false
        member this.booleanType = this.boolTrue --> Boolean
                                  |- this.boolFalse --> Boolean

        
        member this.number = this.negNumber |- this.posNumber
        member this.letter = this.lLetter |- this.uLetter
        
        //correção no id: antes permitia apenas um número ser um identificador.
        member this.id = (this.letter + (this.letter |- this.digit).oneOrMore) --> fun (a,l) -> a::l |> System.String.Concat
                         |- this.letter --> fun a -> (string) a

        //operators:
        //regras de parsing de operações numéricas
        //BUGADO: parser não permite id * id sabe o diabo porque
        member this.calcOp = 
                let multiplicative = production "multiplicative"
                let additive = production "additive"
        
                let number = this.number --> Number
                let id = this.id --> Id
                //esse number acima é para manter todo mundo do mesmo tipo (Exp). Se não usar a regra do scanrat reclama de tipos inconsistentes na mesma regra.

                let add = (this.whitespace.oneOrMore.opt +. (additive |- id) .+ this.whitespace.oneOrMore.opt) .+ ~~"+" + (this.whitespace.oneOrMore.opt +. (multiplicative |- id) .+ this.whitespace.oneOrMore.opt)  --> Add
                let sub = (this.whitespace.oneOrMore.opt +. (additive |- id) .+ this.whitespace.oneOrMore.opt) .+ ~~"-" + (this.whitespace.oneOrMore.opt +. (multiplicative |- id) .+ this.whitespace.oneOrMore.opt) --> Subtract

                let multiply = (this.whitespace.oneOrMore.opt +. (multiplicative |- id ) .+ this.whitespace.oneOrMore.opt) .+ ~~"*" + (this.whitespace.oneOrMore.opt +. (number |- id) .+ this.whitespace.oneOrMore.opt) --> Multiply
                let divide = (this.whitespace.oneOrMore.opt +. (multiplicative |- id) .+ this.whitespace.oneOrMore.opt) .+ ~~"/" + (this.whitespace.oneOrMore.opt +. (number |- id) .+ this.whitespace.oneOrMore.opt) --> Divide

                additive.rule 
                    <- add 
                    |- sub 
                    |- multiplicative
                    //|- id
                
                multiplicative.rule 
                    <- multiply 
                    |- divide 
                    //|- id
                    |- number
                    

                additive

        member this.boolOp = this.booleanType

         

        //regra do assign
        member this.assignRule = (this.whitespace.oneOrMore.opt +. this.id .+ this.whitespace.oneOrMore.opt) .+ ~~":=" 
                                 + (this.whitespace.oneOrMore +. (this.calcOp |- this.boolOp) .+ this.whitespace.oneOrMore.opt) --> Assign

        member this.booleanCondition =  ~~"(" +. this.whitespace.oneOrMore.opt +. this.boolOp .+ this.whitespace.oneOrMore.opt .+ ~~")"


        member this.command = this.loopRule .+ ~~";"
                              //|- this.ifRule .+ ~~";"
                              |- this.assignRule .+ ~~";"

        member this.seqRule = this.command + this.command --> Seq


        member this.XBlockRule =( ~~"{"  + (this.seqRule |- this.command)  + ~~"}") --> fun a -> Block(snd(fst(a))) //--> fun a -> Block (fst(fst(a))) Ele acusa a XBlockRule, seqRule, a command e o looprule de explodirem (é basicamente o trace de execução)

        
        //de loop só tem o while na documentação da IMP:
        member this.loopRule =  //(this.whitespace.oneOrMore.opt + ~~"while" +. this.whitespace.oneOrMore.opt) + this.boolOp + this.XBlockRule --> fun a ->  Loop ((snd(fst(a)),(snd(a))))
                                //~~"while" +.  (this.booleanType) + ~~"do" +. this.XBlockRule
                                ~~"while" +.  (this.booleanType) + this.XBlockRule --> fun a ->  Loop ((snd(fst(a)),(snd(a))))
                               
        





