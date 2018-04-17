module Distances

open Grid
open SixLabors.ImageSharp.PixelFormats
open Colors

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

type Distances with
    
    member this.Max = max this

    member this.NormItem cell = (this.Item cell |> float) / (snd this.Max |> float)

let distancesContent (distances:Distances) cell =
    if distances.Cells.ContainsKey cell then (string distances.[cell]).PadRight 3 else "   "

let floatRgba32 (r:float, g:float, b:float) = Rgba32 (r |> byte, g |> byte, b |> byte)

let shadeColor (dist:Distances) cell =
    let shade = 255.0 - (dist.NormItem cell) * 255.0
    (shade, shade, 255.0) |> floatRgba32

let rainbowShadeWithShift (dist:Distances) cycle shift cell =
    let t =
        match ((dist.Item cell + shift |> float) % cycle) / cycle with
        | x when x < 0.5 -> x * 2.0
        | x -> 1.0 - ((x - 0.5) * 2.0)
    let hue = 360.0 * t
    let (r,g,b) = hsv2rgb (hue, 1.0, 1.0)
    (r * 255.0, g * 255.0, b * 255.0) |> floatRgba32

let rainbowShade (dist:Distances) cycle cell = rainbowShadeWithShift dist cycle 0 cell
