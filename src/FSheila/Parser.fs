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
         | Block of Tipao * Tipao //um block segura um Bloco de comandos seguido de um outro bloco de comandos, ou seja: sequência de comandoss eguindo de um if ou um while ou dois while. É como uma sequência de blocos.
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
         //| Empty //usado p/ quanado há somente um bloco (no longer needed)
         | Enviroment of Dictionary<string, Tipao> //para desempilhar
         // (novos tipos para a p3)
         | Formals of Tipao //seria uma lista de parâmetros? Formals são os parâmetros formais da declaração do proc.
         | Prc of Tipao * Tipao * Tipao //Id, formals e Block
         | Empty  //Problema: do jeito que tá definido a declaração de formals na formalsRule, não pode ser vazio. Prc necessariamente precisa de algo ali no meio pra marcar que tem ou não parâmetro. Isso pode
                  // corrigido usando uma lista de variaveis a serem declaradas.
         

type PEGParser () = 
        //vale a pena lembrar que os operadores --> vão sair; a semântica das operãções vão vir da BPLC

        member this.whitespace = (~~" ").oneOrMore
        member this.linebreak = (~~"\r\n").oneOrMore

        member this.posNumber =  (oneOf "0123456789").oneOrMore --> fun l -> System.String.Concat(l) |> int |> bigint
        member this.negNumber = this.whitespace.oneOrMore.opt +. ~~"(" + ~~"-" +. this.posNumber .+ ~~")" .+ this.whitespace.oneOrMore.opt --> fun a -> -a
                                |- this.whitespace.oneOrMore.opt +. ~~"-" +. this.posNumber .+ this.whitespace.oneOrMore.opt--> fun a -> -a

        member this.digit = oneOf "0123456789"
        member this.lLetter = oneOf "abcdefghijklmnopqrstuvwxyz" --> fun a -> a
        member this.uLetter = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ" --> fun a -> a
        member this.boolTrue = ~~"true" --> fun(a) -> true
        member this.boolFalse = ~~"false" --> fun(a) -> false
        member this.booleanType = (this.boolTrue |- this.boolFalse)

        
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
                  |- this.constRule .+ ~~";" .+ (this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt) + this.validSeq --> ConstBlock
                  |- this.constRule .+ ~~";" .+ (this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt) + this.realSeqVarRule --> ConstBlock
                 
                  
               seqConstRule

        member this.realSeqConstRule =  ~~"const" +. this.seqConstRule .+ this.linebreak.oneOrMore.opt


        member this.decRule = this.realSeqVarRule |- this.realSeqConstRule


        member this.initRule = 
             let boole = this.boolOp //--> Boolexp
             let numExp = this.calcOp //|- boole
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
               let seq = ((this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt) +. (this.assignRule ) .+ this.whitespace.oneOrMore.opt) 
                         + ((this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt)  +.  (seqRule) .+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> Seq (a,b)
               seqRule.rule
                  <-  seq |- this.assignRule
               seqRule


  
        member this.XBlockRule = (( this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + ~~"{" + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt 
                                 + this.whitespace.oneOrMore.opt) +. ( this.seqRule) .+ ( this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + ~~"}" 
                                 + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt))


        ///member this.XBlockRule = //fuck
        //        let blockRule = production "blockRule"
        //        //let simpleCommand =  (this.whitespace.oneOrMore.opt |- this.linebreak.oneOrMore.opt)  +. (this.assignRule |- this.seqRule) .+ (this.whitespace.oneOrMore.opt |- this.linebreak.oneOrMore.opt)
        //        let block = (( this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + ~~"{" + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt) +. (blockRule) .+ ( this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + ~~"}" + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt)) //--> Block 
        //        blockRule.rule
        //            <-
        //               this.realSeqConstRule
        //               |- this.seqRule
        //               |- this.assignRule
        //               |- block
                       //|- blockRule
                       //|- (this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + ~~"{" + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt) +. (this.realSeqVarRule) .+ ( this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + ~~"}" + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt)
        //        blockRule

        //member this.blockRule = 
        

        //de loop só tem o while na documentação da IMP:
        member this.loopRule =  (this.whitespace.oneOrMore.opt + ~~"while" + this.whitespace.oneOrMore.opt) +. this.boolOp + this.XBlockRule--> Loop
                               |- (this.whitespace.oneOrMore.opt + ~~"while" + this.whitespace.oneOrMore.opt) + ~~"(" +  this.whitespace.oneOrMore.opt +. this.boolOp .+ this.whitespace.oneOrMore.opt .+ ~~")"   
                               .+ this.whitespace.oneOrMore.opt  + this.XBlockRule --> Loop                               
        

        member this.commaDot = this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt  + this.whitespace.oneOrMore.opt .+  //~~";" .+
                                  this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt+ this.whitespace.oneOrMore.opt + this.whitespace.oneOrMore.opt

        member this.validSeq =    this.loopRule
                                  |- this.seqRule .+ this.commaDot + this.ifRule + this.seqRule --> fun a -> Seq(fst(fst(a)),Seq(snd(fst(a)),snd(a)))
                                  |- this.seqRule .+ this.commaDot + this.loopRule + this.seqRule --> fun a -> Seq(fst(fst(a)),Seq(snd(fst(a)),snd(a)))
                                  |- this.seqRule .+ this.commaDot + this.ifRule --> Seq
                                  |- this.seqRule .+ this.commaDot + this.loopRule --> Seq
                                  |- this.ifRule
                                  |- this.loopRule
                                  |- this.seqRule
                                  //validSeq são sequências "validas". isso acima aparentemente funciona
        member this.generalRule =  this.decRule |- this.validSeq
                                   //this.assignRule |- this.loopRule |- this.seqRule |- this.ifRule //|- this.calcOp |- this.boolOp

        //Bloco geral pra corpo de procedimento:
        member this.blkRule = (( this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + ~~"{" + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt 
                                 + this.whitespace.oneOrMore.opt) +. (this.generalRule) .+ ( this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + ~~"}" 
                                 + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt))



        //p3
        //Declaração de parâmetros é vista como declaração de blocos
        member this.singleFormalRule = this.id --> fun a -> Formals(Id a)

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
                               |- ~~"proc" + this.whitespace.oneOrMore.opt +. this.id .+ ~~"(" + ~~")" + this.blkRule --> fun a ->  Prc( Id (fst(fst(a))), Empty, snd(a))

