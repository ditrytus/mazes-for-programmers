open Mazes.Core.Grid
open Mazes.Core.Generation.BinaryTree
open Mazes.Core.Generation.Sidewinder
open Mazes.Core.Draw.AsciiRegular
open Mazes.Core.Draw.PngRegular
open Mazes.Core.Distances
open Mazes.Core.Generation.AldousBroder
open Mazes.Core.Draw.PngPolar
open Mazes.Core.Generation.Wilson
open Mazes.Core.Generation.RecursiveBacktracker
open Mazes.Core.Generation.HuntAndKill
open System
open FSharp.Collections.ParallelSeq

[<EntryPoint>]
let main _ =

    let height = 20
    let width = 20

    let grid = prepareRegularGrid height width

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
                        |> Seq.averageBy (fun _ -> (prepareRegularGrid height width |> alg ).DeadEnds |> List.length |> float)
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
            let dist = maze |> Distances.ForRoot {Row=height/2; Column=width/2}
            seq { 0 .. snd dist.Max - 1 } |>
                PSeq.iter (fun i -> maze |> drawPngRegular (sprintf "%s_%04i.png" filenamePrefix i) 10 (rainbowShadeWithShift dist ((snd dist.Max / 2) |> float) i))
            maze |> drawAsciiEmpty |> printfn "\n%s" |> drawNext
        | ConsoleKey.R -> 
            Console.Clear()
            let dist = maze |> Distances.ForRoot {Row=height/2; Column=width/2}
            maze |> drawPngRegular (DateTime.Now.Ticks.ToString() + ".png") 10 (rainbowShade dist ((snd dist.Max) |> float))
            maze |> drawAsciiEmpty |> printfn "\n%s" |> drawNext
        | ConsoleKey.C ->
            Console.Clear()
            let dist = maze |> Distances.ForRoot {Row=height/2; Column=width/2}
            maze |> drawPngRegular (DateTime.Now.Ticks.ToString() + ".png") 10 (shadeColor dist)
            maze |> drawAsciiEmpty |> printfn "\n%s" |> drawNext
        | ConsoleKey.L ->
            Console.Clear()
            let (dist, path) = Mazes.Core.Dijkstra.longestPath maze
            maze |> drawAscii (Mazes.Core.Path.pathContent path (dist |> distancesContent)) |> printfn "\n%s" |> drawNext
        | ConsoleKey.D ->
            Console.Clear()
            let dist = maze |> Distances.ForRoot {Row=0; Column=0}
            let path = maze |> Mazes.Core.Dijkstra.findPath dist {Row=height-1; Column=width-1}
            maze |> drawAscii (Mazes.Core.Path.pathContent path (dist |> distancesContent)) |> printfn "\n%s" |> drawNext
        | ConsoleKey.E ->
            Console.Clear()
            let path = maze |> Mazes.Core.DepthFirstSearch.findPath {Row=0; Column=0} {Row=height-1; Column=width-1}
            maze |> drawAscii (Mazes.Core.Path.pathContentDistance path) |> printfn "\n%s" |> drawNext
        | ConsoleKey.S ->
            Console.Clear()
            maze |> drawWhitePngRegular (DateTime.Now.Ticks.ToString() + ".png") 10
            maze |> drawAsciiEmpty |> printfn "\n%s"
            drawNext ()
        | ConsoleKey.M ->
            Console.Clear()

            let sampleMask =
                array2D [
                    [ 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 0;];
                    [ 0; 0; 0; 0; 0; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0;];
                    [ 0; 0; 0; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 0; 0; 0;];
                    [ 0; 0; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 0; 0;];
                    [ 0; 0; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 0; 0;];
                    [ 0; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 0;];
                    [ 0; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 0;];
                    [ 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
                    [ 1; 1; 1; 1; 1; 1; 1; 1; 1; 0; 0; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
                    [ 1; 1; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 1; 1; 1; 1; 1; 1; 1; 1;];
                    [ 1; 1; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 1; 1; 1; 1; 1; 1; 1; 1;];
                    [ 1; 1; 1; 1; 1; 1; 1; 1; 1; 0; 0; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
                    [ 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
                    [ 0; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 0;];
                    [ 0; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 0;];
                    [ 0; 0; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 0; 0;];
                    [ 0; 0; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 0; 0;];
                    [ 0; 0; 0; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 0; 0; 0;];
                    [ 0; 0; 0; 0; 0; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0;];
                    [ 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 0;];
                ]
                |> Array2D.map (fun v -> v > 0)

            let maskedMaze = grid |> Mazes.Core.Mask.mask (Mazes.Core.Mask.fromArray sampleMask) |> recursiveBacktracker
            
            let dist = maskedMaze |> Distances.ForRoot {Row=7; Column=7}
            maskedMaze |> drawPngRegular (DateTime.Now.Ticks.ToString() + ".png") 10 (shadeColor dist)
            maskedMaze |> drawAsciiEmpty |> printfn "\n%s" |> drawNext
        | ConsoleKey.N ->
            let maskedMaze = grid |> Mazes.Core.Mask.mask (Mazes.Core.Mask.Png.fromFile "mask.png") |> recursiveBacktracker
            maskedMaze |> drawWhitePngRegular (DateTime.Now.Ticks.ToString() + ".png") 10
            maskedMaze |> drawAsciiEmpty |> printfn "\n%s"
        | ConsoleKey.P ->
            preparePolarGrid height width |> recursiveBacktracker |> drawWhitePngPolar (DateTime.Now.Ticks.ToString() + ".png") 10
        | _ ->
            Console.Clear()
            maze |> drawAsciiEmpty |> printfn "%s" |> drawNext

    drawNext ()
    0 // return an integer exit code
