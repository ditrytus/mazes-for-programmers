module Distances

open Grid
open SixLabors.ImageSharp.PixelFormats

type Distances = {
    Root: Cell;
    Cells: Map<Cell, int>;
    } with

    static member forRoot root (grid:Grid) =

        let rec processFrontier frontier distances =

            match frontier with
            | [] -> distances
            | _ ->

                let visitedCells = frontier |> List.fold (fun state cell -> state |> List.append (grid.LinksOf cell |> List.where (fun c -> not (distances.Cells.ContainsKey c)) |> List.map (fun c -> (c, distances.[cell] + 1)))) []
                let newFrontier = visitedCells |> List.map fst
                let newDistances = {distances with Cells = distances.Cells |> Map.toList |> List.append visitedCells |> Map.ofList }
                processFrontier newFrontier newDistances

        {Root = root; Cells = [root, 0] |> Map.ofList} |> processFrontier [root]

    member this.Item cell = this.Cells.[cell]

    member this.Set cell distance = {this with Cells = this.Cells |> Map.add cell distance}

let max dist = dist.Cells |> Map.toList |> List.maxBy snd

type Distances with member this.Max = max this

let distancesContent (distances:Distances) cell =
    if distances.Cells.ContainsKey cell then (string distances.[cell]).PadRight 3 else "   "

let shadeColor (dist:Distances) cell =
    let shade = ((dist.Item cell |> float) / (snd dist.Max |> float)) * 255.0
    Rgba32 (shade |> byte, shade |> byte, 255 |> byte)