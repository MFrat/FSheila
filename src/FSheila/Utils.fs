﻿module FSheila.Utils

let printIntro =
    printfn "  ______    ______   _               _   _          "
    printfn " |  ____|  /  ____| | |__          <_> | |         "
    printfn " | |  _   |  (___   |  __ \    ___   _  | |   __ _  "
    printfn " | | (_)  \____  \  | |  | |  / _ \ | | | |  / _` | "
    printfn " | |       ____)  | | |  | | |  __/ | | | | | (_| | "
    printfn " |_|      |______/  |_|  |_|  \___| |_| |_|  \__,_| "
    printfn "                                                    "

let printStacks X S C =
    printfn "X = %A" (X)
    printfn "S = %A" (S)
    printfn "C = %A" (C)