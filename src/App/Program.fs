﻿// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    Library.Say.hello argv.[0]
    0 // return an integer exit code
