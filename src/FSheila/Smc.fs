// Saiba mais sobre F# em http://fsharp.org
// Veja o projeto 'F# Tutorial' para obter mais ajuda.
module FSheila.Smc

open ScanRat
open FSheila
open Parser
open System.Collections.Generic


//Protótipo de SMC Machine:
//S = value Stacks;
//M uma função que dada uma variável Var retorna o valor guardado na memória (pode ser uma função que varre um array que simula essa memoria, e essa função retorna o valor correspondente 
//C = Pilha de controle onde os comandos serão empilhados.
//a ideia é que o resultado do parsing seja atirado para dentro da pilha C e recursivamente seja resolvido até que C fique vazia
type Stack = 
    | StackContents of Exp list

let push x (StackContents contents) =   
    StackContents (x::contents)

let pop (StackContents contents) = 
    match contents with 
    | top::rest -> 
        let newStack = StackContents rest
        (top,newStack)
    | [] -> 
        failwith "Stack underflow"

let S = StackContents []
let C = StackContents []

let rec calculator exp = //a ideia parece ser exatamente isso
    match exp with
    | Add (a, b) -> (calculator a) + (calculator b) //|> push Add S
    | Subtract (a, b) -> (calculator a) - (calculator b) //|> push Subtract S
    | Multiply (a, b) -> (calculator a) * (calculator b) //|> push Multiply S
    | Divide (a, b) -> (calculator a) / (calculator b) //|> push Divide S
    | Number a ->  a //|> push a C

let rec calcbool exp = //a ideia parece ser exatamente isso
    match exp with
    | And (a, b) -> (calcbool a) && (calcbool b) //|> push And S
    | Or (a, b) -> (calcbool a) || (calcbool b) //|> push Or S
    | Neg a -> not(calcbool a) //|> push Neg S
    | Boolean a -> a //|> push a C

//structs costumam ser mais eficientes que classes quando com poucos membros
//https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/structures
type SMC = //parece ser melhor transformar isso num tipo record
    struct
       val S : Stack  
       val M : Dictionary<string,Exp> //M é um dicionário onde a chave é uma string e valor é uma Exp(booleano ou numérico) //Exp list  //criar uma função que mapeie uma var -> conteúdo na memória (um inteiro ou um booleano, de acordo com o que for definido para a variável.   //não gostei de Exp list, mas..
       val C : Stack 
       //new() = {S = [], M = [], C = []} //isso eu acho que eu já ganho por padrão. A inicialização default da estrutura consiste nas três composições estando vazias a princípio.
    end

//regras da SMC
//manipulação da memória
let getFromMemory (s:SMC) (var:string) = s.M.Item(var) //retorna o valor de M associado á variável var

let addToMemory (s:SMC) (var:string) (e:Exp) = s.M.Add(var,e)

//regra de introdução do comando Assign
//let varIntro (smc:SMC) =
//    match (pop(smc.C)) with
//    | Assign (a,b) -> push(a smc.S)

let getFromParser exp (smc:SMC) =
    match exp with
    | Success r -> push r.value (smc.C)
    | Failure _ -> failwith "Parsing falhou!"

let math exp = 
    match exp with
    | Success s -> calculator s.value
    | Failure f -> failwith "deu bosta então se foda"   


    //de acordo com as transições em SMC (descritas nas páginas 15, 16 e 17