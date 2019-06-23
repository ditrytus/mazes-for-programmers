namespace Mazes.Core.Generation

module RecursiveBacktracker =

    open Mazes.Core.Grid
    open Mazes.Core.Utils

    let recursiveBacktracker (grid:Grid<_>):Grid<_> =

        let rec carvePath (stack:Cell list) (visited:Cell list) (grid:Grid<_>) =

            match stack with
            | [] -> grid
            | head::tail ->

                match head |> grid.NeighboursOf |> List.except (Seq.ofList visited) |> randomItem with
                | None -> grid |> carvePath tail visited
                | Some cell -> grid.Link head cell |> carvePath (cell::stack) (cell::visited)

        let randomCell = grid.RandomCell
        grid |> carvePath [randomCell] [randomCell]