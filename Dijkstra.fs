module Dijkstra

open Grid
open Distances

let findPath (dist:Distances) goal (grid:Grid) =
    let rec findNext currentPath goal =
        match dist.[goal] with
        | 0 -> currentPath
        | d ->
            let next = grid.LinksOf goal |> List.where (fun l -> dist.[l] = d - 1) |> List.item 0
            findNext (next::currentPath) next
    findNext [goal] goal

let pathContent path content cell = if path |> List.contains cell then content cell else "   "