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
         | Init of string * Exp
         | If of boolExp * Cmd
         | Loop of boolExp * Cmd // Cmd list (ou não -->) //um bloco é visto pelo ScanRat como uma lista de comandos.
         | Seq of Cmd * Cmd 

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

             let boolean = this.whitespace.oneOrMore.opt +. this.booleanType --> Boolean
             //se não me engano and tem precedência sobre or.
             let ourAnd = (this.whitespace.oneOrMore.opt +. andOp .+ this.whitespace.oneOrMore.opt) .+ ~~"and" + (this.whitespace.oneOrMore.opt +. orOp .+ this.whitespace.oneOrMore.opt) --> And
             let ourOr = (this.whitespace.oneOrMore.opt +. andOp .+ this.whitespace.oneOrMore.opt) .+ ~~"or" + (this.whitespace.oneOrMore.opt +. orOp .+ this.whitespace.oneOrMore.opt) --> Or
             let ourNeg = this.whitespace.oneOrMore.opt + ~~"~" +. (boolean) --> Neg
                        |- ( this.whitespace.oneOrMore.opt + ~~"~" + this.whitespace.oneOrMore.opt + this.whitespace.oneOrMore.opt + ~~"(" +. (andOp |- orOp) .+ this.whitespace.oneOrMore.opt .+ ~~")")  --> Neg

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

        member this.initRule =
                //boolexp tá bugado
             let boole = this.boolOp --> Boolexp
             let numExp = this.calcOp  //|- boole
             //let boolEx = 
             let initRule = production "initRule"
             let oneAssignExp = ~~"init" +. (this.whitespace.oneOrMore +. this.id .+ this.whitespace.oneOrMore.opt) .+ ~~"=" + (this.whitespace.oneOrMore +. (numExp) .+ this.whitespace.oneOrMore.opt) --> Init
             let oneAssignBoolex = ~~"init" +. (this.whitespace.oneOrMore +. this.id .+ this.whitespace.oneOrMore.opt) .+ ~~"=" + (this.whitespace.oneOrMore +. (boole) .+ this.whitespace.oneOrMore.opt) --> Init
             //NOTA: para o uso de múltiplos assigns é necessário ter um espaço como definido no this.whitespace.oneOrMore (vide documentação de IMP).
             let moreAssigns = ~~"," +. (this.whitespace.oneOrMore +. this.id .+ this.whitespace.oneOrMore.opt) .+  ~~"=" + (this.whitespace.oneOrMore +. (numExp |- boole) .+ this.whitespace.oneOrMore.opt) --> Init //.+ ~~","+. (this.whitespace.oneOrMore.opt +. this.id .+ this.whitespace.oneOrMore.opt)
             let moreAssignsBool = ~~"," +. (this.whitespace.oneOrMore +. this.id .+ this.whitespace.oneOrMore.opt) .+  ~~"=" + (this.whitespace.oneOrMore +. (numExp |- boole) .+ this.whitespace.oneOrMore.opt) --> Init
             initRule.rule
                //o init não precisa ser levado como dado importante para o processo de semântica pela definição da regra acima (note que o mesmo ocorre com "var" e "const").
                <- (oneAssignExp |- oneAssignBoolex) + (moreAssigns |- moreAssignsBool)
             initRule
         

        //regra do assign
        //nota: por enquanto só funciona para atribuições numéricas (inclusive atribuição de expressões numéricas). Falta eu fazer rodar pra operações booleanas tbm.
        member this.assignRule =
             //let boole = this.boolOp
             let numExp = this.calcOp //|- boole
             //let boolEx = this.boolOp
             let assignRule = production "assignRule"
             let oneAssignExp = (this.whitespace.oneOrMore.opt +. this.id .+ this.whitespace.oneOrMore.opt) .+ ~~":=" + (this.whitespace.oneOrMore +. numExp .+ this.whitespace.oneOrMore.opt) --> Assign
             //let oneConstBoolExp =  ~~"=" + (this.whitespace.oneOrMore + Boolexp .+ this.whitespace.oneOrMore.opt) --> Assign
             //NOTA: para o uso de múltiplos assigns é necessário ter um espaço como definido no this.whitespace.oneOrMore (vide documentação de IMP).
             //NOTA2: assigns são únicos (diferentes de init). Múltiplos assigns devem fazer uso do comando de sequência.
             //let moreAssigns =  ~~";" +. (this.whitespace.oneOrMore +. this.id .+ this.whitespace.oneOrMore.opt) .+  ~~":=" + (this.whitespace.oneOrMore +. numExp .+ this.whitespace.oneOrMore.opt) --> Assign //.+ ~~","+. (this.whitespace.oneOrMore.opt +. this.id .+ this.whitespace.oneOrMore.opt)
             assignRule.rule
                //o init não precisa ser levado como dado importante para o processo de semântica pela definição da regra acima (note que o mesmo ocorre com "var" e "const").
                <- (oneAssignExp) //+ moreAssigns.oneOrMore.opt)
             assignRule
         
        //BUGADO
        member this.ifRule =
               let command = this.command
               let boolExp = this.boolOp
               let block = this.blockRule

               let ifRule = production "ifRule"
               let aIf = ~~"if" + boolExp + this.whitespace.oneOrMore + command + this.whitespace.oneOrMore + ~~"else" +. command 
               let aIfBlock =  ~~"if" +. boolExp + block .+ ~~"else" +. command
               let aIfBlock2 =  ~~"if" +. boolExp + command .+ ~~"else" +. block
               let aIfBlock3 = ~~"if" +. boolExp + block .+ ~~"else" +. block
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
                    <- (this.whitespace.oneOrMore.opt + ~~"{" + this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt) +. this.seqRule .+ ( this.whitespace.oneOrMore.opt + this.linebreak.oneOrMore.opt + this.whitespace.oneOrMore.opt + ~~"}" + this.whitespace.oneOrMore.opt)
                blockRule
        member this.command = this.blockRule |- this.assignRule //|- this.assignRule //|- this.loopRule.oneOrMore

        //de loop só tem o while na documentação da IMP:
        member this.loopRule = (this.whitespace.oneOrMore.opt + ~~"while" + this.whitespace.oneOrMore) +. this.boolOp + this.command  --> Loop

       

[<EntryPoint>]
let main argv = 
    let valueOf r =
        match r with
        | Success s -> s.value
        | Failure _ -> failwith "parse failed"
    //nota: tornar possível 3 + 4 <= 5 - 8, ou seja, operações matemáticas dentro de booleanas
    let testGrammar = new PEGParser()
    //let teste = parse testGrammar.calcOp "-21 + 5555 + 3 + 4 * 666 /   5"
    //let teste = parse testGrammar.boolOp "true and 3==3 and true and false";
    //let teste = parse testGrammar.boolOp "~(true and ~false)"
    //let teste = parse testGrammar.boolOp "~(true and ~(3<>4))"
    //let teste2 = parse testGrammar.boolOp "true and false or false and true"
    //let teste = parse testGrammar.boolOp "3<=4 and 4<>5 or true and false and 666  <= 4 or 1981> 2007"
    //let teste = parse testGrammar.varRule "var x , y , z, a"
    //let teste = parse testGrammar.constRule "const abc , x , y , a69"
    //let teste = parse testGrammar.initRule "init x = 2 , y = 555*6/8 , abhe =   1981"
    //let teste = parse testGrammar.assignRule "a := 444*58- 69 ; u := 1+2*333 + 8"
    //let teste = parse testGrammar.boolOp "~(true and ~(3<>4 and false))" 
    //let teste = parse testGrammar.blockRule "{ sexo := 3}"
    //embora o teste abaixo para mim nao deveria funcionar, boa sorte se quiser fazer a sequencia funcionar de boa de outra forma
    //let teste = parse testGrammar.seqRule "a:= 333"
    //let teste = parse testGrammar.seqRule "a := 333 ; b := 444 ; ccc := 69-66*8 ; cas := 0 "//; b:= 555"
    //let teste = parse testGrammar.seqRule " a:= 333;b := 888 * 7 ; c := 666*777/8"
    let teste = parse testGrammar.loopRule "while 3<>4 { 
                                                  sheila3 := 555+8 ; 
                                                  sheila := 9999 ; ati := 999*555 ;


                                                  aaa := 1+9-5*8  
                                                  }"
    //let teste2 = parse testGrammar.ifRule "if true a := 3 else a:=4"
    //let ae = valueOf teste
    printfn "%A" teste
    //printfn "%A" teste
    let sheila = Console.ReadLine()
    printfn "%A" sheila
    //printfn "%A" argv
    0 // retornar um código de saída inteiro