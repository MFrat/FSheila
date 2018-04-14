namespace File1

//módulo que contêm a ebnf de IMP em PEG usando ScanRat

open ScanRat

        
type PEGParser () =
    member this.number = oneOf "0123456789" --> fun a -> int(a) - int('0')
    member this.negNumber = ~~"-" + this.number --> fun a -> int (-snd(a))- int('0')
    member this.addRule =  (this.number |- this.negNumber) + ~~"+" + (this.number |- this.negNumber) --> fun (a,b) -> fst(a) + b
    member this.subRule = (this.number |- this.negNumber) + ~~"+" + (this.number |- this.negNumber) --> fun (a,b) -> fst(a) - b
    member this.divRule = (this.number |- this.negNumber) + ~~"/" + (this.number |- this.negNumber) --> fun (a,b) -> fst(a) / b
    member this.mulRule = (this.number |- this.negNumber) + ~~"+" + (this.number |- this.negNumber) --> fun (a,b) -> fst(a) * b
    //member this.spaceRule = matchChar(" ")

