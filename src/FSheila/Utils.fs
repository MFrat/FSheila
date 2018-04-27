module FSheila.Utils

let printIntro =
    printfn "  ______    ______   _               _   _          "
    printfn " |  ____|  /  ____| | |__          <_> | |         "
    printfn " | |  _   |  (___   |  __ \    ___   _  | |   __ _  "
    printfn " | | (_)  \____  \  | |  | |  / _ \ | | | |  / _` | "
    printfn " | |       ____)  | | |  | | |  __/ | | | | | (_| | "
    printfn " |_|      |______/  |_|  |_|  \___| |_| |_|  \__,_| "
    printfn "                                                    "

let printStacks S C =
    printfn "S = %A" (S)
    printfn "C = %A" (C)
    printfn "\n"

let printSMC S M C =
    printfn "S = %A" (S)
    printfn "M = %A" (M)
    printfn "C = %A" (C)