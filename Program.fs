// Learn more about F# at http://fsharp.org

open Grid
open BinaryTree
open Sidewinder
open DrawAscii
open DrawPng
open Distances
open Dijkstra
open AldousBroder
open Wilson
open System
open FSharp.Collections.ParallelSeq

[<EntryPoint>]
let main argv =

    let height = 10
    let width = 10

    let grid = prepareGrid height width

    let rec drawNext _ =
        let maze = grid |> wilson
        match (Console.ReadKey ()).Key with
        | ConsoleKey.Escape -> ()
        | ConsoleKey.G ->
            Console.Clear()
            let filenamePrefix = DateTime.Now.Ticks.ToString()
            let dist = maze |> Distances.forRoot (height/2, width/2)
            seq { 0 .. snd dist.Max - 1 } |>
                PSeq.iter (fun i -> maze |> drawPng (sprintf "%s_%04i.png" filenamePrefix i) 10 (rainbowShadeWithShift dist ((snd dist.Max / 2) |> float) i))
            maze |> drawAsciiEmpty |> printfn "\n%s" |> drawNext
        | ConsoleKey.R -> 
            Console.Clear()
            let dist = maze |> Distances.forRoot (height/2, width/2)
            maze |> drawPng (DateTime.Now.Ticks.ToString() + ".png") 10 (rainbowShade dist ((snd dist.Max / 2) |> float))
            maze |> drawAsciiEmpty |> printfn "\n%s" |> drawNext
        | ConsoleKey.C ->
            Console.Clear()
            let dist = maze |> Distances.forRoot (height/2, width/2)
            maze |> drawPng (DateTime.Now.Ticks.ToString() + ".png") 10 (shadeColor dist)
            maze |> drawAsciiEmpty |> printfn "\n%s" |> drawNext
        | ConsoleKey.L ->
            Console.Clear()
            let (dist, path) = Dijkstra.longestPath maze
            maze |> drawAscii (pathContent path (dist |> distancesContent)) |> printfn "\n%s" |> drawNext
        | ConsoleKey.D ->
            Console.Clear()
            let dist = maze |> Distances.forRoot (0, 0)
            let path = maze |> Dijkstra.findPath dist (height - 1, width - 1)
            maze |> drawAscii (pathContent path (dist |> distancesContent)) |> printfn "\n%s" |> drawNext
        | ConsoleKey.S ->
            Console.Clear()
            maze |> drawWhitePng (DateTime.Now.Ticks.ToString() + ".png") 10
            maze |> drawAsciiEmpty |> printfn "\n%s"
            drawNext ()
        | _ ->
            Console.Clear()
            maze |> drawAsciiEmpty |> printfn "%s" |> drawNext

    drawNext ()
    0 // return an integer exit code
