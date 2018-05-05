module RecursiveBacktracker

open Grid

let recursiveBacktracker (grid:Grid):Grid =

    let rec carvePath (stack:Cell list) (visited:Cell list) (grid:Grid) =

        match stack with
        | [] -> grid
        | head::tail ->

            match head |> grid.NeighboursOf |> List.except (Seq.ofList visited) |> Utils.randomItem with
            | None -> grid |> carvePath tail visited
            | Some cell -> grid.Link head cell |> carvePath (cell::stack) (cell::visited)

    let randomCell = grid.RandomCell
    grid |> carvePath [randomCell] [randomCell]