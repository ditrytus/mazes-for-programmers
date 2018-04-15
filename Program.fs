﻿// Learn more about F# at http://fsharp.org

open Grid
open BinaryTree
open Sidewinder
open DrawAscii
open DrawPng
open Distances
open Dijkstra
open System

[<EntryPoint>]
let main argv =

    let height = 10
    let width = 10

    let grid = prepareGrid height width

    let rec drawNext _ =
        let maze = grid |> sidewinder
        match (Console.ReadKey ()).Key with
        | ConsoleKey.Escape -> ()
        | ConsoleKey.D ->
            Console.Clear()
            let dist = maze |> Distances.forRoot (0, 0)
            let path = maze |> Dijkstra.findPath dist (height - 1, width - 1)
            maze |> drawAscii (pathContent path (dist |> distancesContent)) |> printfn "\n%s" |> drawNext
        | ConsoleKey.S ->
            Console.Clear()
            maze |> drawPng (DateTime.Now.Ticks.ToString() + ".png") 10
            maze |> drawAsciiEmpty |> printfn "\n%s"
            drawNext ()
        | _ ->
            Console.Clear()
            maze |> drawAsciiEmpty |> printfn "%s" |> drawNext

    drawNext ()
    0 // return an integer exit code
