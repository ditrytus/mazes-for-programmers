// Learn more about F# at http://fsharp.org

open Grid
open BinaryTree
open Sidewinder
open DrawAscii
open System

[<EntryPoint>]
let main argv =

    let grid = prepareGrid 30 50

    let rec drawNext _ =
        match (Console.ReadKey ()).Key with
        | ConsoleKey.Escape -> ()
        | _ ->
            Console.Clear()
            grid |> binaryTree |> drawAscii |> printfn "%s" |> drawNext

    drawNext ()
    0 // return an integer exit code
