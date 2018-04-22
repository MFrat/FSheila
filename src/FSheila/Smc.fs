// Saiba mais sobre F# em http://fsharp.org
// Veja o projeto 'F# Tutorial' para obter mais ajuda.
module FSheila.Smc

open ScanRat
open FSheila
open Parser
open System.Collections.Generic //daqui vem o dicionario


//Protótipo de SMC Machine:
//S = value Stacks;
//M uma função que dada uma variável Var retorna o valor guardado na memória (pode ser uma função que varre um array que simula essa memoria, e essa função retorna o valor correspondente 
//C = Pilha de controle onde os comandos serão empilhados.
//a ideia é que o resultado do parsing seja atirado para dentro da pilha C e recursivamente seja resolvido até que C fique vazia (ou):

//type 'a ourStack =
//    | Stack of 'a list

//type  CmdStack = 
//   | StackContents of Cmd list

//type ValueStack = //pilha de valores, o S de SMC
//  | StackContents of Exp list

//type Stacks = //nossas pilhas ou são pilhas de valores (S -> ValueStack) ou pilhas de controle (C -> CmdStack). Eu tinha tido uma ideia que era separar 2 pilhas como nesse tipo e asism teria que ter 2 push e 2 pop, mas um genérico resolve o problema
//de forma bem mais elegante
//    | ValueStack
//    | CmdStack

//let push x (stack: 'a ourStack) contents  = //push genérico
//   Stack (x::contents)

//let pop (stack : 'a Stack) contents = 
//    match contents with 
//    | top::rest -> 
//        let newStack = Stack rest
//        (top,newStack)
//    | [] -> 
//        failwith "Stack underflow"

//let S = StackContents []
//let C = StackContents []

//structs costumam ser mais eficientes que classes quando com poucos membros
//https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/structures
type SMC = //parece ser melhor transformar isso num tipo record
    struct
       val S : Stack<Cmd> //aqui ficam valores resultantes de processamentos de comandos na pilha de controle (strings, bool e numeros)
       val M : Dictionary<string,Cmd> //M é um dicionário onde a chave é uma string e valor é uma Exp(booleano ou numérico) //Exp list  //criar uma função que mapeie uma var -> conteúdo na memória (um inteiro ou um booleano, de acordo com o que for definido para a variável.   //não gostei de Exp list, mas..
       val C : Stack<Cmd> //a pilha de conrole possui apenas comandos. tem que ver isso melhor 
       //new() = {S = [], M = [], C = []} //isso eu acho que eu já ganho por padrão. A inicialização default da estrutura consiste nas três composições estando vazias a princípio.
       val Especial : Stack<string>
    end

//regras da SMC
//manipulação da memória
let getFromMemory (s:SMC) (var:string) = s.M.Item(var) //retorna o valor de M associado á variável var

let addToMemory (s:SMC) (var:string) (e:Cmd) = s.M.Add(var,e)

//regra de introdução do comando Assign
//NOTA: por chatices do caralho de tipagem em F#, parece ser impossível se livrar desses identificadores que constroem um tipo para fins de manipulações (como armazenar na pilha). Nesse caso, o id da variavel que ficar em var vai ter que ter o Id
//o acompanhando, um número resutante terá um Number o acompanhando e a gente vai ter que dar um jeito pra tirar esse troço em tempo de execução (printar, p.e).
//real eu acho que todas as operações podem ficar em uma única função, mas a gente tem que organizar segundo as regras da SMC.
let varIntro (smc:SMC) =
    match (smc.C.Pop()) with
    | Assign (a, b) -> smc.S.Push(Id a) //falta empilhar o b de volta em C... aqui é o correspondente à introdução do Assign (a regra C := I do plotkin)

let getFromParser exp (smc:SMC) : int =
    match exp with
    | Success r ->  smc.C.Push(r.value)
    | Failure _ -> failwith "Parsing falhou!"
    
    0

    //de acordo com as transições em SMC (descritas nas páginas 15, 16 e 17

    
let rec calculator (smc:SMC) = //a ideia parece ser exatamente isso
    printfn "%A" smc.C.ToString
    while smc.C.Count = 0 do
        match smc.C.Pop() with
        | Add (a, b) -> smc.Especial.Push("Add")// (calculator a) + (calculator b) //|> push Add S
        | Subtract (a, b) -> smc.Especial.Push("Subtract")//(calculator a) - (calculator b) //|> push Subtract S
        | Multiply (a, b) -> smc.Especial.Push("Multiply")//(calculator a) * (calculator b) //|> push Multiply S
        | Divide (a, b) -> smc.Especial.Push("Divide")//(calculator a) / (calculator b) //|> push Divide S
        | And (a, b) -> smc.Especial.Push("And")//(calculator a) && (calculator b) //|> push And S
        | Or (a, b) -> smc.Especial.Push("Or")//(calculator a) || (calculator b) //|> push Or S
        | Neg a -> smc.Especial.Push("Neg")//not(calculator a) //|> push Neg S
        | Number a ->  smc.S.Push(Number a)
        | Boolean a -> smc.S.Push(Boolean a)

let printstacks (smc:SMC) = 
    printfn "S = %A" smc.S.ToString