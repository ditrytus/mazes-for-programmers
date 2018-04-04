module BinaryTree

open Grid
open Utils

let binaryTree (g:Grid) =

    let linkNeighbour grid cell =

        let neighbours c =
            [North; East] 
            |> List.map (fun dir -> c |> goTo g dir) 
            |> List.choose (fun c -> c)

        match cell |> neighbours |> random with
        | Some neighbour -> grid |> link cell neighbour
        | None -> grid

    g.Cells |> List.fold linkNeighbour g 