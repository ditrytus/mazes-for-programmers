// Learn more about F# at http://fsharp.org

open Grid
open BinaryTree
open DrawAscii

[<EntryPoint>]
let main argv =
    prepareGrid 6 6 |> binaryTree |> drawAscii |> printfn "%s"
    0 // return an integer exit code
