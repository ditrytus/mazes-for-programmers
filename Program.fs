// Learn more about F# at http://fsharp.org

open Grid
open BinaryTree
open Sidewinder
open DrawAscii
open DrawPng
open System

[<EntryPoint>]
let main argv =

    let grid = prepareGrid 10 10

    let rec drawNext _ =
        let maze = grid |> sidewinder
        match (Console.ReadKey ()).Key with
        | ConsoleKey.Escape -> () 
        | ConsoleKey.S ->
            maze |> drawPng (DateTime.Now.Ticks.ToString() + ".png") 10
            maze |> drawAscii |> printfn "\n%s"
            drawNext ()
        | _ ->
            Console.Clear()
            maze |> drawAscii |> printfn "%s" |> drawNext

    drawNext ()
    0 // return an integer exit code
