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
         | If of Tipao * Tipao * Tipao 
         | Loop of Tipao * Tipao
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
         | ConstInit of string * Tipao
         | VarInit of string * Tipao
         | SeqDec of Tipao * Tipao //sequência de declarações. Uma regra que tem um var seguido de const, ex. Pra isso, uma regra correspondente tem que ser criada no parser
         | VarBlock of Tipao * Tipao //bloco declarado por declaração de var
         | ConstBlock of Tipao * Tipao //bloco declarado por declaração de const.
         //Declarations' orders
         | XVar //Talvez precisemos de dois tipos distintos para var e const por possuírem uma semântica diferente.
         | XConst
         | XInit
         //novos tipos p controle de blocos
         | XVarBlock
         | XConstBlock
         | XBlock 
         | Enviroment of Dictionary<string, Tipao> //para desempilhar
         // (novos tipos para a p3)
         | Print of Tipao
         | For of Tipao //Formals são os parâmetros formais da declaração do proc e de fun.
         | Act of Tipao //Actuals são os parâmetros passados efetivamente para procs e funs.
         | Prc of Tipao * Tipao * Tipao //Id, formals e Block
         | Prcf of Tipao * Tipao //procedimentos sem parâmetros (Id e Block).
         | VarDec of string //Declaração de variáveis a nível de Módulo (ou seria of Tipao?)
         | ConstDec of string //declaração de constantes a nível de código (mesma ressalva acima).
         | Init of Tipao * Tipao
         | Dec of Tipao * Tipao //talvez saia MODULE
         | Blk of Tipao  //To usando esse bloco pra bloco de coisas que podem estar dentro de um módulo
         | Fun of Tipao * Tipao * Tipao //Tipos p/ funções que retornam valores
         | Funf of Tipao * Tipao //função sem parâmetros
         | Cal of Tipao * Tipao //Chamada de procedimentos e funçõe com parâmetros
         | Calf of Tipao //Chamada de procedimentos e funções sem parâmetros
         | Ret of Tipao //Ret indica o valor a ser retornado de uma função
         | Blkf of Tipao * Tipao //SHEILA ROOT
         //minhas bosta ai
         | Abs of Tipao * Tipao //formals and block
         | Absf of Tipao //block
         | XCal
         | XCalf
         | XBlk
         | Empty

type PEGParser () = 
        //vale a pena lembrar que os operadores --> vão sair; a semântica das operãções vão vir da BPLC

        member this.whitespace = (~~" ").oneOrMore
        member this.linebreak = (~~"\r\n").oneOrMore

        member this.posNumber =  (oneOf "0123456789").oneOrMore --> fun l -> System.String.Concat(l) |> int |> bigint
        member this.negNumber = this.whitespace.oneOrMore.opt +. ~~"(" + ~~"-" +. this.posNumber .+ ~~")" .+ this.whitespace.oneOrMore.opt --> fun a -> -a
                                |- this.whitespace.oneOrMore.opt +. ~~"-" +. this.posNumber .+ this.whitespace.oneOrMore.opt--> fun a -> -a

        member this.digit = oneOf "0123456789"
        member this.lLetter = oneOf "_-abcdefghijklmnopqrstuvwxyz" --> fun a -> a
        member this.uLetter = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ" --> fun a -> a
        member this.boolTrue = ~~"true" --> fun(a) -> true
        member this.boolFalse = ~~"false" --> fun(a) -> false
        member this.booleanType = (this.boolTrue |- this.boolFalse)

        
        member this.number = this.negNumber |- this.posNumber
        member this.letter = this.lLetter |- this.uLetter
        
        //correção no id: antes permitia apenas um número ser um identificador.
        member this.id = (this.letter + (this.letter |- this.digit).oneOrMore) --> fun (a,l) -> a::l |> System.String.Concat
                         |- this.letter --> fun a -> (string) a
        
        member this.value =  this.number --> Number
                            |- this.whitespace.oneOrMore.opt +. this.booleanType  .+ this.whitespace.oneOrMore.opt --> Boolean
                            |- this.id --> Id

        //operators:
        //regras de parsing de operações numéricas
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

        member this.boolOp =
             let andOp = production "andOp"
             let orOp = production "orOp"

             let boolean = this.whitespace.oneOrMore.opt +. this.booleanType  .+ this.whitespace.oneOrMore.opt --> Boolean
             let number = this.number --> Number
             let id = this.id --> Id
             let ourAnd = (this.whitespace.oneOrMore.opt +. (boolean |- andOp) .+ this.whitespace.oneOrMore.opt) .+ ~~"and" + (this.whitespace.oneOrMore.opt +. (boolean |- orOp) .+ this.whitespace.oneOrMore.opt) --> And
             let ourOr = (this.whitespace.oneOrMore.opt +. andOp .+ this.whitespace.oneOrMore.opt) .+ ~~"or" + (this.whitespace.oneOrMore.opt +. orOp .+ this.whitespace.oneOrMore.opt) --> Or
             let ourNeg = this.whitespace.oneOrMore.opt + ~~"~" +. (boolean |- id) --> Neg
                        |- ( this.whitespace.oneOrMore.opt + ~~"~" + this.whitespace.oneOrMore.opt + this.whitespace.oneOrMore.opt + ~~"(" +. (andOp |- orOp) .+ this.whitespace.oneOrMore.opt .+ ~~")")  --> Neg

             let eqOp = (this.whitespace.oneOrMore.opt +. ( number |- boolean |- id ) .+ this.whitespace.oneOrMore.opt) + ~~"==" + (this.whitespace.oneOrMore.opt +. ( number |- boolean |- id) .+ this.whitespace.oneOrMore.opt) --> fun (a,b) -> Eq (fst(a),b)
             let lebOp = (this.whitespace.oneOrMore.opt +. ( number |- id) .+ this.whitespace.oneOrMore.opt) + ~~"<" + (this.whitespace.oneOrMore.opt +. ( number |- id) .+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> Leb (fst(a),b)
             let leqOp = (this.whitespace.oneOrMore.opt +. ( number |- id) .+ this.whitespace.oneOrMore.opt) + ~~"<=" + (this.whitespace.oneOrMore.opt +. ( number |- id) .+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> Leq (fst(a),b)
             let gebOp = (this.whitespace.oneOrMore.opt +. ( number |- id) .+ this.whitespace.oneOrMore.opt) + ~~">" + (this.whitespace.oneOrMore.opt +. ( number |- id) .+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> Geb (fst(a),b)
             let geqOp = (this.whitespace.oneOrMore.opt +. ( number |- id) .+ this.whitespace.oneOrMore.opt) + ~~"=>" + (this.whitespace.oneOrMore.opt +. ( number |- id) .+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> Geq (fst(a),b)
             let neqOp = (this.whitespace.oneOrMore.opt +. ( number |- boolean  |- id) .+ this.whitespace.oneOrMore.opt) + ~~"<>" + (this.whitespace.oneOrMore.opt +. ( number |- boolean  |- id) .+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> Neq (fst(a),b)
             let compareOp = eqOp |- lebOp |- leqOp |- gebOp |- geqOp |- neqOp
             //a princípio, não é permitido 3 + 4 < 4 * 5, por exemplo (operações matemáticas dentro de comparações numéricas).
             andOp.rule
                 <- boolean
                 |- ourAnd 
                 |- ourOr
                 |- ourNeg
                 |- compareOp
                 //|- this.id --> Id

             orOp.rule
                 <- boolean
                 |- ourAnd
                 |- ourOr
                 |- ourNeg
                 |- compareOp
                 //|- this.id --> Id
                
             andOp



        member this.varRule = 
               //segundo a regra abaixo eu forço ter pelo menos um espaço depoi da keyword "var"
               (this.whitespace.oneOrMore.opt +. this.id .+ this.whitespace.oneOrMore.opt) .+ ~~"=" + (this.whitespace.oneOrMore +. (this.calcOp |- this.boolOp) .+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> (VarInit(a,b)) 
               |- (this.whitespace.oneOrMore.opt +. this.id .+ this.whitespace.oneOrMore.opt) .+ ~~"=" + (this.whitespace.oneOrMore +. (this.id --> Id) .+ this.whitespace.oneOrMore.opt) --> fun(a,b) ->  (VarInit(a,b))

        member this.seqVarRule =  
               let seqVarRule = production "seqVarRule"
               let seq = ((this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt) +. this.varRule .+ this.whitespace.oneOrMore.opt) .+ ~~"," 
                         + ((this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt)  +. seqVarRule.+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> VarBlock (a,b)
               seqVarRule.rule
                  <-  seq
                      //|- this.varRule .+ ~~";" .+ (this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt) + this.validFunSeq --> VarBlock
                      |- this.varRule .+ ~~";" .+ (this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt) + this.validSeq --> VarBlock
               seqVarRule

        member this.realSeqVarRule =  ~~"var" +. this.seqVarRule .+ this.linebreak.oneOrMore.opt

         
        member this.constRule = 
               //segundo a regra abaixo eu forço ter pelo menos um espaço depois da keyword "const"
                (this.whitespace.oneOrMore.opt +. this.id .+ this.whitespace.oneOrMore.opt) .+ ~~"=" + (this.whitespace.oneOrMore +. (this.calcOp |- this.boolOp) .+ this.whitespace.oneOrMore.opt) --> ConstInit
                                  |- (this.whitespace.oneOrMore.opt +. this.id .+ this.whitespace.oneOrMore.opt) .+ ~~"=" + (this.whitespace.oneOrMore +. (this.id --> Id) .+ this.whitespace.oneOrMore.opt) --> ConstInit

        member this.seqConstRule =  
               let seqConstRule = production "seqConstRule"
               let seq = ((this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt) +. (this.constRule) .+ this.whitespace.oneOrMore.opt) .+ ~~"," + 
                         ((this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt)  +. seqConstRule.+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> ConstBlock (a,b)
               seqConstRule.rule
                  <- seq 
                  //|- this.constRule .+ ~~";" .+ (this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt) + this.validFunSeq --> ConstBlock
                  |- this.constRule .+ ~~";" .+ (this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt) + this.validSeq --> ConstBlock
                  |- this.constRule .+ ~~";" .+ (this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt) + this.realSeqVarRule --> ConstBlock
                 
                  
               seqConstRule

        member this.realSeqConstRule =  ~~"const" +. this.seqConstRule .+ this.linebreak.oneOrMore.opt


        member this.decRule = this.realSeqVarRule |- this.realSeqConstRule

         

        //regra do assign
        member this.assignRule = (this.whitespace.oneOrMore.opt +. this.id .+ this.whitespace.oneOrMore.opt) .+ ~~":=" 
                                 + (this.whitespace.oneOrMore +. (this.calcOp |- this.boolOp) .+ this.whitespace.oneOrMore.opt) .+ ~~";" --> Assign


        member this.ifRule =
               //let command = this.assignRule
               //adicionado parentização no "if"
               ~~"if" +. (this.whitespace.oneOrMore.opt +. ~~"(" +. this.whitespace.oneOrMore.opt +. this.boolOp .+ this.whitespace.oneOrMore.opt .+ ~~")" .+ this.whitespace.oneOrMore.opt ) +
                         this.assignRule .+ ~~"else" + this.XBlockRule --> fun a -> If (fst(fst(a)),snd(fst(a)),snd(a))

               |- ~~"if" +. (this.whitespace.oneOrMore.opt +. ~~"(" +. this.whitespace.oneOrMore.opt +. this.boolOp .+ this.whitespace.oneOrMore.opt .+ ~~")" .+ this.whitespace.oneOrMore.opt ) 
               + this.XBlockRule .+ ~~"else" + this.XBlockRule -->  fun a -> If (fst(fst(a)),snd(fst(a)),snd(a))

               |- ~~"if" +. (this.whitespace.oneOrMore.opt +. ~~"(" +. this.whitespace.oneOrMore.opt +. this.boolOp .+ this.whitespace.oneOrMore.opt .+ ~~")" .+ this.whitespace.oneOrMore.opt ) + 
                         this.assignRule.+ ~~"else" + this.assignRule --> fun a -> If (fst(fst(a)),snd(fst(a)),snd(a))
               |- ~~"if" +. (this.whitespace.oneOrMore.opt +. ~~"(" +. this.whitespace.oneOrMore.opt +. this.boolOp .+ this.whitespace.oneOrMore.opt .+ ~~")" .+ this.whitespace.oneOrMore.opt ) + this.XBlockRule .+ ~~"else" +
                         this.assignRule .+ ~~";" -->  fun a -> If (fst(fst(a)),snd(fst(a)),snd(a))
              
        //regra de sequência 
        member this.seqRule =
               let seqRule = production "seqRule"
               let seq = ((this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt) +. (this.assignRule |- this.printRule |- this.callRule) .+ this.whitespace.oneOrMore.opt) 
                         + ((this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt)  +.  (seqRule) .+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> Seq (a,b)
               seqRule.rule
                  <-  seq |- this.assignRule |- this.printRule |- this.callRule  |- this.retRule
               seqRule

        member this.seqFunRule =
               let seqFunRule = production "seqFunRule"
               let seq = ((this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt) +. (this.assignRule |- this.printRule |- this.callRule |- this.retRule) .+ this.whitespace.oneOrMore.opt) 
                         + ((this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt)  +.  (seqFunRule) .+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> Seq (a,b)
               seqFunRule.rule
                  <-  seq |- this.assignRule |- this.printRule |- this.callRule //|- this.retRule //retRule é a regra de retorno.
               seqFunRule



  
        member this.XBlockRule = (( this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + ~~"{" + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt 
                                 + this.whitespace.oneOrMore.opt) +. ( this.seqRule) .+ ( this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + ~~"}" 
                                 + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt))

 
        

        //de loop só tem o while na documentação da IMP:
        member this.loopRule =  (this.whitespace.oneOrMore.opt + ~~"while" + this.whitespace.oneOrMore.opt) +. this.boolOp + this.XBlockRule--> Loop
                               |- (this.whitespace.oneOrMore.opt + ~~"while" + this.whitespace.oneOrMore.opt) + ~~"(" +  this.whitespace.oneOrMore.opt +. this.boolOp .+ this.whitespace.oneOrMore.opt .+ ~~")"   
                               .+ this.whitespace.oneOrMore.opt  + this.XBlockRule --> Loop                               
        

        member this.commaDot = this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt  + this.whitespace.oneOrMore.opt .+  //~~";" .+
                                  this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt+ this.whitespace.oneOrMore.opt + this.whitespace.oneOrMore.opt

        member this.validSeq =    this.loopRule + this.retRule --> Seq  
                                  |- this.seqRule .+ this.commaDot + this.ifRule + this.seqRule --> fun a -> Seq(fst(fst(a)),Seq(snd(fst(a)),snd(a)))
                                  |- this.seqRule .+ this.commaDot + this.loopRule + this.seqRule --> fun a -> Seq(fst(fst(a)),Seq(snd(fst(a)),snd(a)))
                                  |- this.seqRule .+ this.commaDot + this.ifRule --> Seq
                                  |- this.seqRule .+ this.commaDot + this.loopRule --> Seq
                                  |- this.retRule
                                  |- this.ifRule
                                  |- this.loopRule
                                  |- this.seqRule
                                  //validSeq são sequências "validas". isso acima aparentemente funciona
        member this.generalRule =  this.decRule |- this.validSeq

        //comandos para funções
        member this.validFunSeq =    this.loopRule + this.retRule --> Seq
                                  |- this.seqRule .+ this.commaDot + this.ifRule + this.seqRule --> fun a -> Seq(fst(fst(a)),Seq(snd(fst(a)),snd(a)))
                                  |- this.seqRule .+ this.commaDot + this.loopRule + this.seqRule --> fun a -> Seq(fst(fst(a)),Seq(snd(fst(a)),snd(a)))
                                  |- this.seqRule .+ this.commaDot + this.ifRule --> Seq
                                  |- this.seqRule .+ this.commaDot + this.loopRule --> Seq
                                  |- this.ifRule
                                  |- this.retRule
                                  |- this.loopRule
                                  |- this.seqRule
                                  //validSeq são sequências "validas". isso acima aparentemente funciona
        member this.generalFunRule =  this.decRule |- this.validFunSeq

        //member this.generalFunRule = this.retRule
        //                             |- (this.decRule |- this.validSeq) + this.retRule --> Seq //corpo de uma função necessita ser encerrado por um "return statement".

        //Bloco geral pra corpo de procedimento:
        member this.blkRule = (( this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + ~~"{" + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt 
                                 + this.whitespace.oneOrMore.opt) +. (this.generalRule) .+ ( this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + ~~"}" 
                                 + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt)) --> Blk


        member this.blkFunRule = (( this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + ~~"{" + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt 
                                 + this.whitespace.oneOrMore.opt) +. (this.generalRule) .+ ( this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + ~~"}" 
                                 + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt)) --> Blk //generalFunRule
       

        


        //p3
        //Declaração de parâmetros é vista como declaração de blocos
        member this.singleFormalRule = this.id --> fun a -> For(Id a)

        member this.formalsRule = 
               let formalsRule = production "formalsRule"
               let formals = ((this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt) +. this.singleFormalRule .+ this.whitespace.oneOrMore.opt) .+ ~~"," 
                             + ((this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt)  +. formalsRule .+ this.whitespace.oneOrMore.opt) --> Seq //Sequência de formals, mas essa Seq é a mesma de comando.
               formalsRule.rule
                  <-  formals 
                      |- this.singleFormalRule
               formalsRule

        //this.formalsRule é a regra que permmite o parsing dos parâmetros formais de uma função.
        member this.procRule = ~~"proc" + this.whitespace.oneOrMore.opt +. this.id .+ ~~"(" + this.formalsRule + ~~")" + this.blkRule --> fun a -> Prc (Id(fst(fst(fst(a)))),(snd(fst(fst(a)))), snd(a))
                               |- ~~"proc" + this.whitespace.oneOrMore.opt +. this.id .+ ~~"(" + ~~")" + this.blkRule --> fun a ->  Prcf( Id (fst(fst(a))), snd(a))

        member this.moreProcsRule = 
               let moreProcsRule = production "moreProcsRule"
               let moreProcs = this.procRule .+ this.whitespace.oneOrMore.opt .+ this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + moreProcsRule --> fun a -> Seq(fst(fst(a)),snd(a))
               moreProcsRule.rule
                    <- moreProcs
                    |- this.procRule
               moreProcsRule

        //regras de declaração de variáveis e constantes a nível de módulos
        //regra para inicialização de variável/constante:
        //TODO init espera um string ou um Tipao (id)? Eu acho que é um Id, pois a semântica dele é procurar uma variável já declarada para amarrar um valor a ela
        //------------ estranho
        //seq = ((this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt) +. this.varRule .+ this.whitespace.oneOrMore.opt) .+ ~~"," 
        //                 + ((this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt)  +. seqVarRule.+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> VarBlock (a,b)
        member this.eq = this.whitespace.oneOrMore.opt + ~~"=" +. this.whitespace.oneOrMore.opt
        member this.idToId = (this.id --> Id)
        member this.init = ~~"init" + this.whitespace.oneOrMore.opt +. this.idToId +  this.eq + (this.calcOp |- this.boolOp) --> fun a -> Init(fst(fst(a)),snd(a))

        member this.initRule = 
               let initRule = production "initRule"
               let manyInits = this.init 
                               .+ this.whitespace.oneOrMore.opt + ~~"," +. initRule 
               initRule.rule
                   <- manyInits
                   |- this.init
               initRule
        //regra de declaração de módulos
        member this.constDecModuleRule = 
               let constDecModuleRule = production "constDecModuleRule"
               let moreDecs = ~~"const" + this.whitespace.oneOrMore.opt +. this.id + ~~"," + constDecModuleRule --> fun a -> Seq( ConstDec(fst(fst(a))), ConstDec(snd(fst(a))))
               constDecModuleRule.rule
                   <-  moreDecs
                    |-  ~~"const" + this.whitespace.oneOrMore.opt +. this.id --> fun a -> ConstDec(a)
               constDecModuleRule
        //--------------------------------------
        member this.varDecModuleRule = 
               let varDecModuleRule = production "varDecModuleRule"
               let moreDecs = ~~"var" + this.whitespace.oneOrMore.opt +. this.id + ~~"," + varDecModuleRule --> fun a -> Seq( VarDec(fst(fst(a))), VarDec(snd(fst(a))))
               varDecModuleRule.rule
                   <-  moreDecs
                    |-  ~~"var" + this.whitespace.oneOrMore.opt +. this.id --> fun a -> VarDec(a)
                    |-  this.constDecModuleRule
               varDecModuleRule


        //Regras para o parsing de funções que retornam valores
        member this.retRule = this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + ~~"return" +. this.whitespace.oneOrMore.opt +. this.value .+ ~~";"--> Ret

        member this.funRule = ~~"fun" + this.whitespace.oneOrMore.opt +. this.id .+ ~~"(" + this.formalsRule + ~~")" + this.blkFunRule  --> fun a -> Fun (Id(fst(fst(fst(a)))),(snd(fst(fst(a)))), snd(a))
                               |- ~~"fun" + this.whitespace.oneOrMore.opt +. this.id .+ ~~"(" + ~~")" + this.blkFunRule --> fun a ->  Funf( Id (fst(fst(a))), snd(a))

        member this.moreFunsRule = 
               let moreFunsRule = production "moreFunsRule"
               let moreFuns = this.funRule .+ this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt +. moreFunsRule
               moreFunsRule.rule
                    <- moreFuns
                    |- this.funRule
               moreFunsRule


        member this.procFunRule = 
               let moreFunsRule = production "moreFunsRule"
               let moreProcs = this.procRule .+ this.whitespace.oneOrMore.opt .+ this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + moreFunsRule --> fun a -> Seq(fst(fst(a)),snd(a))
               let moreFuns = this.funRule .+ this.whitespace.oneOrMore.opt .+ this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + moreFunsRule --> fun a -> Seq(fst(fst(a)),snd(a))
               moreFunsRule.rule
                    <- moreFuns
                    |- moreProcs
                    |- this.funRule
                    |- this.procRule
               moreFunsRule
       
        member this.moduleBlkRule = (this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt) +. (((this.varDecModuleRule |- this.constDecModuleRule) .+ (this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt) + this.initRule .+ (this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt) ) --> Seq)
                                    .+ (this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt) + (this.procFunRule) 
                                    .+ (this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt) --> Dec

        member this.moduleRule = ~~"module" +  this.whitespace.oneOrMore.opt +. this.id +. this.moduleBlkRule .+ ~~"end" //--> Exec

        //Regra de parsing para várias chamadas após a declaração do módulo
        member this.execCallRule = 
               let execCallRule = production "execCallRule"
               let seq = ((this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt) +. (this.callRule) .+ this.whitespace.oneOrMore.opt) 
                         + ((this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt)  +.  (execCallRule) .+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> Seq (a,b)
               execCallRule.rule
                  <-  seq |- this.callRule 
               execCallRule
        
        member this.sheilaRule = this.moduleRule + this.execCallRule --> Blkf

        //regra para parsing de chamadas de funções/procedimentos
        //Actuals são parâmetros efetivamente passados para a função. Eles tem que bater *exatamente* com os parâmetros formais declarados no procedimento/função.
        member this.singleActualsRule = this.value --> fun a -> Act(a)

        member this.actualsRule =
               let actualsRule = production "actualsRule"
               let actuals = ((this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt) +. this.singleActualsRule .+ this.whitespace.oneOrMore.opt) .+ ~~"," 
                             + ((this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt)  +. actualsRule .+ this.whitespace.oneOrMore.opt) --> Seq 
               actualsRule.rule
                  <-  actuals 
                  |- this.singleActualsRule
               actualsRule

        member this.callRule = (this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt) +. this.id .+ ~~"(" + this.actualsRule .+ ~~")" .+ ~~";"
                               --> fun a -> Cal (Id (fst(a)), snd(a))
                               |- (this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt) +. this.id .+ ~~"(" + ~~")" .+ ~~";" --> fun a -> Calf (Id(fst(a)))

        //Regra do print
        member this.printRule = ~~"print" + this.whitespace.oneOrMore.opt + ~~"(" +. this.whitespace.oneOrMore.opt +. this.value .+ this.whitespace.oneOrMore.opt .+ ~~")" .+ ~~";" --> Print

        //Declaração de módulos e execução de função à posteriori
        //member this.execRule = this.moduleRule + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.callRule
