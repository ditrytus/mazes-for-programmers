module Wilson

open Grid
open Utils

let wilson (grid:Grid) =

    let rec processPath unvisited path (grid:Grid) =

        match unvisited |> List.isEmpty with
        | true -> grid
        | false -> 

            match path with
            | [] -> failwith "Path should never be empty!"
            | head::tail ->

                match unvisited |> List.contains head with
                | false -> tail
                           |> List.fold (fun (g:Grid, c) cell -> (g.Link c cell, cell)) (grid, head)
                           |> fst
                           |> processPath (unvisited |> List.except (path |> Seq.ofList)) [grid.RandomCell]
                | true -> 

                    match tail |> List.contains head with
                    | true -> grid |> processPath unvisited (tail |> List.skipWhile (fun t -> t <> head))
                    | false ->

                        match grid.NeighboursOf head |> randomItem with
                        | None -> failwith "Every cell has a neighbour!"
                        | Some next -> grid |> processPath unvisited (next::path)

    grid |> processPath (grid.Cells |> List.except [grid.RandomCell] ) [grid.RandomCell]