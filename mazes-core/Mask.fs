module Mask

open Grid

type Mask = Cell -> bool

let mask (mask:Mask) (grid:Grid<'d>) : Grid<'d> =
    { grid with
            Cells = grid.Cells |> List.where mask;
            Neighbourhood = grid.Neighbourhood
                |> Map.filter (fun cell _ -> mask cell)
                |> Map.map (fun _ dirMap ->
                    dirMap
                    |> Map.map (fun _ cell ->
                        match cell with
                        | None -> None
                        | Some cell -> if mask cell then Some cell else None )) }

let fromArray (array:bool[,]) {Row=row;Column=col} = array.[row,col]