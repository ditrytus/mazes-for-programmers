// Learn more about F# at http://fsharp.org

open Grid
open BinaryTree
open Sidewinder
open DrawAscii
open System

[<EntryPoint>]
let main argv =

    let rec drawNext _ =
        match (Console.ReadKey ()).Key with
        | ConsoleKey.Escape -> ()
        | _ ->
            Console.Clear()
            prepareGrid 20 20 |> sidewinder |> drawAscii |> printfn "%s" |> drawNext

    drawNext ()
    0 // return an integer exit code
