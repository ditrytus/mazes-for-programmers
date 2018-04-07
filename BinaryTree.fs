module BinaryTree

open Grid
open Utils

let binaryTree (grid:Grid) =

    let linkNeighbour (grid:Grid) cell =

        let neighbours c =
            [North; East] 
            |> List.map (fun dir -> c |> grid.GoTo dir) 
            |> List.choose id

        match cell |> neighbours |> random with
        | Some neighbour ->  grid.Link cell neighbour
        | None -> grid

    grid.Cells |> List.fold linkNeighbour grid 