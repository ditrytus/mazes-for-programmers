namespace Mazes.Core.Generation

module BinaryTree = 

    open Mazes.Core.Grid
    open Mazes.Core.Utils

    let binaryTree (grid:Grid<_>) =

        let linkNeighbour (grid:Grid<_>) cell =

            let neighbours c =
                [North; East] 
                |> List.map (fun dir -> c |> grid.GoTo dir) 
                |> List.choose id

            match cell |> neighbours |> randomItem with
            | Some neighbour ->  grid.Link cell neighbour
            | None -> grid

        grid.Cells |> List.fold linkNeighbour grid 