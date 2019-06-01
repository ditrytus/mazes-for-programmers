module AldousBroder

open Grid
open Utils

let aldousBroder (grid:Grid<_>) = 

    let rec processSetp cell left (grid:Grid<_>) =

        match left with
        | 0 -> grid
        | _ -> 

            match grid.NeighboursOf cell |> randomItem with
            | None -> failwith "Cell has no neighbours!"

            | Some next when grid.LinksOf next |> List.isEmpty -> grid.Link cell next |> processSetp next (left-1)
            | Some next -> grid |> processSetp next left

    grid |> processSetp grid.RandomCell (grid.Size-1)
