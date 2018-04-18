module HuntAndKill

open Grid
open Utils


let huntAndKill (grid:Grid) = 

    let rec processStep cell (grid:Grid) =

        let isNotVisited cell = grid.LinksOf cell |> Seq.isEmpty
        let isVisited cell = cell |> isNotVisited |> not
        let visitedNeighboursOf cell = cell |> grid.NeighboursOf |> Seq.filter isVisited

        match grid.NeighboursOf cell |> List.where isNotVisited |> randomItem with
        | Some next -> grid.Link cell next |> processStep next
        | None -> 

            let next = grid.Cells
                       |> Seq.filter isNotVisited
                       |> Seq.filter (fun c -> visitedNeighboursOf c |> Seq.isEmpty |> not)
                       |> Seq.tryItem 0

            match next with
            | None -> grid
            | Some next ->

                match visitedNeighboursOf next |> List.ofSeq |> randomItem with
                | None -> failwith "There can not be no visited neighbours because it was checked before."
                | Some neighbour -> grid.Link next neighbour |> processStep next


    grid |> processStep grid.RandomCell
