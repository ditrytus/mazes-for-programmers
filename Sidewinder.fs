module Sidewinder

open Grid
open Utils

let sidewinder (grid:Grid) =

    let rand = System.Random ();
    
    let rec processRow (currentRun : Cell list) (remaining : Cell list) (grid : Grid) =

        match remaining with
        | [] -> grid
        | currentCell::rest ->

            let newRun = currentCell::currentRun

            let carveNorth noNorth (grid:Grid) =

                match random newRun with
                | None -> grid
                | Some randomCell ->

                    match randomCell |> grid.GoTo North with
                    | None -> noNorth
                    | Some northCell ->
                        grid.Link randomCell northCell |> processRow [] rest

            match rest with
            | [] -> carveNorth grid grid
            | nextCell::_ ->

                let carveEast (grid:Grid) =
                    (grid.Link currentCell nextCell) |> processRow newRun rest

                match rand.Next(2) with
                | 0 -> carveEast grid
                | _ -> carveNorth (carveEast grid) grid
                        
    grid.Rows |> Seq.fold (fun grid row -> grid |> processRow [] (row |> List.ofSeq)) grid