module FSheila.Parser

open ScanRat

type Cmd =
         //id é apenas uma string que representa o nome da variável
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
         | Number of bigint
         | Boolean of bool
         | Id of string
         | CmdAdd
         | CmdSubtract
         | CmdDivide 
         | CmdMultiply 
         | CmdAnd  
         | CmdOr  
         | CmdNeg  
         | CmdEq  
         | CmdLeb  
         | CmdLeq 
         | CmdGeb 
         | CmdGeq 
         | CmdNeq
         | CmdAssign
         | CmdIf
         | CmdLoop
         //novos tipos para a P2.
         | SeqDec of Cmd * Cmd //sequência de declarações. Uma regra que tem um var seguido de const, ex. Pra isso, uma regra correspondente tem que ser criada no parser
         | VarInit of string * Cmd
         | DclVars of Cmd //seqûência de vars na mesma linha.
         | ConstInit of string * Cmd
         | DclConsts of Cmd
         | Assign of string * Cmd
         | Init of string * Cmd
         | CmdInit
         | CmdVar //Talvez precisemos de dois tipos distintos para var e const por possuírem uma semântica diferente.
         | CmdConst
         //novos tipos p controle de blocos
         | VarBlock of Cmd * Cmd //bloco declarado por declaração de var
         | ConstBlock of Cmd * Cmd //bloco declarado por declaração de const.
         | Block of Cmd * Cmd
         | Empty //usado p/ quanado há somente um bloco

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
             //se não me engano and tem precedência sobre or.
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


        //commands:
        //funciona, mas não sei se é o ideal: abaixo a regra de parsing de var <ident>^+
        member this.varRule = 
               let var = production "var"
               //segundo a regra abaixo eu forço ter pelo menos um espaço depoi da keyword "var"
               let oneVarInit = (this.whitespace.oneOrMore.opt +. this.id .+ this.whitespace.oneOrMore.opt) .+ ~~"=" + (this.whitespace.oneOrMore +. (this.calcOp |- this.boolOp) .+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> (VarInit(a,b)) //VarBlock não vem aqui
                                 |- (this.whitespace.oneOrMore.opt +. this.id .+ this.whitespace.oneOrMore.opt) .+ ~~"=" + (this.whitespace.oneOrMore +. (this.id --> Id) .+ this.whitespace.oneOrMore.opt) --> fun(a,b) ->  (VarInit(a,b)) //VarBlock não vem aqui
               var.rule
                  <- oneVarInit
               var

        member this.seqVarRule =  
               let seqVarRule = production "seqVarRule"
               let seq = ((this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt) +. this.varRule .+ this.whitespace.oneOrMore.opt) .+ ~~"," + ((this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt)  +. seqVarRule.+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> VarBlock (a,b)
               seqVarRule.rule
                  <-  seq
                      |- this.varRule --> fun a -> VarBlock(a,Empty)
               seqVarRule

        member this.realSeqVarRule = this.linebreak.oneOrMore.opt +. ~~"var" +. this.seqVarRule .+ this.linebreak.oneOrMore.opt

         
        member this.constRule = 
               let constAtr = production "const"
               //segundo a regra abaixo eu forço ter pelo menos um espaço depois da keyword "const"
               let oneConstInit = (this.whitespace.oneOrMore.opt +. this.id .+ this.whitespace.oneOrMore.opt) .+ ~~"=" + (this.whitespace.oneOrMore +. (this.calcOp |- this.boolOp) .+ this.whitespace.oneOrMore.opt) --> ConstInit
                                  |- (this.whitespace.oneOrMore.opt +. this.id .+ this.whitespace.oneOrMore.opt) .+ ~~"=" + (this.whitespace.oneOrMore +. (this.id --> Id) .+ this.whitespace.oneOrMore.opt) --> ConstInit
               constAtr.rule
                  <- oneConstInit
               constAtr

        member this.seqConstRule =  
               let seqConstRule = production "seqConstRule"
               let seq = ((this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt) +. (this.constRule) .+ this.whitespace.oneOrMore.opt) .+ ~~"," + 
                         ((this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt)  +. seqConstRule.+ this.whitespace.oneOrMore.opt) --> fun(a,b) -> ConstBlock (a,b)
               seqConstRule.rule
                  <- seq |- 
                  this.constRule --> fun a -> ConstBlock(a,Empty)
                  |- this.constRule + this.cmdBlockRule --> ConstBlock
               seqConstRule

        member this.realSeqConstRule =  this.linebreak.oneOrMore.opt +. ~~"const" +. this.seqConstRule .+ this.linebreak.oneOrMore.opt

        member this.decRule = //sequência de declarações.
               let decRule = production "decRule"
               decRule.rule
                  <- //não sei se precisa criar uma sequência específica aqui.
                  (this.realSeqVarRule .+ (this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt) .+ ~~";" .+ 
                  (this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt) + decRule) --> Seq
                  |- (this.realSeqVarRule .+ (this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt) .+ ~~";" .+ 
                  (this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt) + this.realSeqConstRule) --> Seq
                  |- (this.realSeqConstRule .+ (this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt) .+ ~~";" .+ 
                  (this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt) + this.realSeqVarRule) --> Seq
                  |- (this.realSeqConstRule .+ (this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt) .+ ~~";" .+ 
                  (this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt) + this.realSeqConstRule) --> Seq
                  //|- (decRule .+ (this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt) .+ ~~";" .+ 
                  //(this.linebreak.oneOrMore.opt +. this.whitespace.oneOrMore.opt |- this.whitespace.oneOrMore.opt) + this.cmdBlockRule) --> Seq

                  //|- (this.realSeqConstRule .+ ~~";" +  this.realSeqVarRule) --> Seq 
                  |- this.realSeqConstRule |- this.realSeqVarRule
               decRule

        member this.initRule = //obs: init incompleto.
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
        member this.assignRule =
             let boole = this.boolOp 
             let numExp = this.calcOp
             let assignRule = production "assignRule"
             let oneAssignExp = (this.whitespace.oneOrMore.opt +. this.id .+ this.whitespace.oneOrMore.opt) .+ ~~":=" + (this.whitespace.oneOrMore +. (numExp |- boole) .+ this.whitespace.oneOrMore.opt) --> Assign
             assignRule.rule
                <- ( oneAssignExp) //+ moreAssigns.oneOrMore.opt)
             assignRule
         

        member this.ifRule =
               //let command = this.assignRule
               //adicionado parentização no "if"
               let boolExp = (this.whitespace.oneOrMore.opt +. ~~"(" +. this.whitespace.oneOrMore.opt +. this.boolOp .+ this.whitespace.oneOrMore.opt .+ ~~")" .+ this.whitespace.oneOrMore.opt )
               let block = this.cmdBlockRule

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

        //regra do bloco de comandos, parece estar 100%
        //member this.blockRule = 
        //       let blockRule = production "blockRule"
        //        //let simpleCommand =  (this.whitespace.oneOrMore.opt |- this.linebreak.oneOrMore.opt)  +. (this.assignRule |- this.seqRule) .+ (this.whitespace.oneOrMore.opt |- this.linebreak.oneOrMore.opt)
        //        blockRule.rule
        //            <- (this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + ~~"{" + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt) +. (this.seqRule) .+ ( this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + ~~"}" + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt) //--> Block
        //              |- (this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + ~~"{" + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt) +. (this.realSeqVarRule) .+ ( this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + ~~"}" + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt)
        //               |- this.assignRule
        //        blockRule
        member this.command = this.cmdBlockRule

        member this.cmdBlockRule = //fuck
                let blockRule = production "blockRule"
                //let simpleCommand =  (this.whitespace.oneOrMore.opt |- this.linebreak.oneOrMore.opt)  +. (this.assignRule |- this.seqRule) .+ (this.whitespace.oneOrMore.opt |- this.linebreak.oneOrMore.opt)
                let block = (( this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + ~~"{" + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt) +. (blockRule) .+ ( this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + ~~"}" + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt)) //--> Block 
                //let moreBlocks
                blockRule.rule
                    <-    //block .+ ~~"{" + block --> Block 
                       block --> fun a -> Block(a, Empty)
                       |- this.realSeqVarRule
                       //|- block +  blockRule --> Block 
                       |- this.seqRule 
                       //|- blockRule
                       //|- (this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + ~~"{" + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt) +. (this.realSeqVarRule) .+ ( this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + ~~"}" + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt)
                blockRule

        //member this.blockRule = 

        //de loop só tem o while na documentação da IMP:
        member this.loopRule = (this.whitespace.oneOrMore.opt + ~~"while" + this.whitespace.oneOrMore) +. this.boolOp + this.command --> Loop
                               |- (this.whitespace.oneOrMore.opt + ~~"while" + this.whitespace.oneOrMore) +  this.whitespace.oneOrMore + ~~"(" +  this.whitespace.oneOrMore +. this.boolOp .+ this.whitespace.oneOrMore .+ ~~")" .+  this.whitespace.oneOrMore  + this.command --> Loop
        
        member this.generalRule =  this.assignRule |- this.loopRule |- this.seqRule |- this.ifRule //|- this.calcOp |- this.boolOp