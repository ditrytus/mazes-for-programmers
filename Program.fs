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
open RecursiveBacktracker
open HuntAndKill
open System
open FSharp.Collections.ParallelSeq

[<EntryPoint>]
let main _ =

    let height = 20
    let width = 20

    let grid = prepareGrid height width

    let rec drawNext _ =
        let maze = grid |> recursiveBacktracker
        match (Console.ReadKey ()).Key with
        | ConsoleKey.Escape -> ()
        | ConsoleKey.T ->
            Console.Clear()
            [
                ("Binary tree", binaryTree);
                ("Sidewinder", sidewinder);
                ("Aldous-Broder", aldousBroder);
                ("Wilson", wilson);
                ("Hunt and Kill", huntAndKill);
                ("Recursive Backtracker", recursiveBacktracker)
            ]
            |> List.map (fun (name, alg) ->
                printfn "Running %s..." name
                let avg = {0..10}
                        |> Seq.averageBy (fun _ -> (prepareGrid height width |> alg ).DeadEnds |> List.length |> float)
                (name, avg)
                )
            |> List.iter (fun (name, avg) ->
                let size = width * height
                printfn "%s: %i%% (%i/%i)" name (int ((avg/float size)*100.0)) (int avg) size
               )
            |> drawNext
        | ConsoleKey.G ->
            Console.Clear()
            let filenamePrefix = DateTime.Now.Ticks.ToString()
            let dist = maze |> Distances.ForRoot (height/2, width/2)
            seq { 0 .. snd dist.Max - 1 } |>
                PSeq.iter (fun i -> maze |> drawPng (sprintf "%s_%04i.png" filenamePrefix i) 10 (rainbowShadeWithShift dist ((snd dist.Max / 2) |> float) i))
            maze |> drawAsciiEmpty |> printfn "\n%s" |> drawNext
        | ConsoleKey.R -> 
            Console.Clear()
            let dist = maze |> Distances.ForRoot (height/2, width/2)
            maze |> drawPng (DateTime.Now.Ticks.ToString() + ".png") 10 (rainbowShade dist ((snd dist.Max) |> float))
            maze |> drawAsciiEmpty |> printfn "\n%s" |> drawNext
        | ConsoleKey.C ->
            Console.Clear()
            let dist = maze |> Distances.ForRoot (height/2, width/2)
            maze |> drawPng (DateTime.Now.Ticks.ToString() + ".png") 10 (shadeColor dist)
            maze |> drawAsciiEmpty |> printfn "\n%s" |> drawNext
        | ConsoleKey.L ->
            Console.Clear()
            let (dist, path) = Dijkstra.longestPath maze
            maze |> drawAscii (Path.pathContent path (dist |> distancesContent)) |> printfn "\n%s" |> drawNext
        | ConsoleKey.D ->
            Console.Clear()
            let dist = maze |> Distances.ForRoot (0, 0)
            let path = maze |> Dijkstra.findPath dist (height - 1, width - 1)
            maze |> drawAscii (Path.pathContent path (dist |> distancesContent)) |> printfn "\n%s" |> drawNext
        | ConsoleKey.E ->
            Console.Clear()
            let path = maze |> DepthFirstSearch.findPath (0, 0) (height - 1, width - 1)
            maze |> drawAscii (Path.pathContentDistance path) |> printfn "\n%s" |> drawNext
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
