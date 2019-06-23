namespace Mazes.Core

module HuntAndKill = 

    open Grid
    open Utils

    let huntAndKill (grid:Grid<_>) = 

        let rec processStep (unvisited:Cell list) cell (grid:Grid<_>) =

            match unvisited with
            | [] -> grid
            | _ ->

                let processNextStep next = processStep (unvisited |> List.except [next]) next

                match grid.NeighboursOf cell |> Set.ofList |> Set.intersect (unvisited |> Set.ofList) |> Set.toList |> randomItem with
                | Some next -> grid.Link cell next |> processNextStep next
                | None -> 

                    let isVisited cell = unvisited |> Seq.contains cell |> not
                    let visitedNeighboursOf cell = cell |> grid.NeighboursOf |> Seq.filter isVisited

                    let next = unvisited
                               |> Seq.where (visitedNeighboursOf >> Seq.isEmpty >> not)
                               |> Seq.item 0

                    match visitedNeighboursOf next |> List.ofSeq |> randomItem with
                    | None -> failwith "There can not be no visited neighbours because it was checked before."
                    | Some neighbour -> grid.Link next neighbour |> processNextStep next

        let randomCell = grid.RandomCell
        grid |> processStep (grid.Cells |> List.except [randomCell]) randomCell
