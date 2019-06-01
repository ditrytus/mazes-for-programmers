module Dijkstra

open Grid
open Distances

let findPath (dist:Distances) goal (grid:Grid<_>) =
    let rec findNext currentPath goal =
        match dist.[goal] with
        | None -> currentPath
        | Some 0 -> currentPath
        | Some d ->
            let next = grid.LinksOf goal |> List.where (fun l -> match dist.[l] with | None -> false | Some dl -> dl = d - 1) |> List.item 0
            findNext (next::currentPath) next
    findNext [goal] goal

let longestPath grid =
    let start = grid |> Distances.ForRoot {Row=0; Column=0} |> Distances.max |> fst
    let dist = grid |> Distances.ForRoot start
    let goal = dist |> Distances.max |> fst
    (dist, findPath dist goal grid)