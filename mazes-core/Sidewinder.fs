namespace Mazes.Core.Generation

module Sidewinder =

    open Mazes.Core.Grid
    open Mazes.Core.Utils

    let sidewinder (grid:Grid<_>) =

        let rand = System.Random ();
        
        let rec processRow (currentRun : Cell list) (remaining : Cell list) (grid : Grid<_>) =

            match remaining with
            | [] -> grid
            | currentCell::rest ->

                let newRun = currentCell::currentRun

                let carveNorth noNorth (grid:Grid<_>) =

                    match randomItem newRun with
                    | None -> grid
                    | Some randomCell ->

                        match randomCell |> grid.GoTo North with
                        | None -> noNorth grid
                        | Some northCell -> grid.Link randomCell northCell |> processRow [] rest

                match rest with
                | [] -> carveNorth id grid

                | nextCell::_ ->

                    let carveEast (grid:Grid<_>) = (grid.Link currentCell nextCell) |> processRow newRun rest

                    match rand.Next(2) with
                    | 0 -> carveNorth carveEast grid
                    | _ -> carveEast grid
                            
        grid.Rows |> Seq.fold (fun grid row -> grid |> processRow [] (row |> List.ofSeq)) grid