// Learn more about F# at http://fsharp.org

open Grid
open BinaryTree

[<EntryPoint>]
let main argv =
    prepareGrid 4 4 |> binaryTree |> ignore
    0 // return an integer exit code
