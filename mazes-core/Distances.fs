module Distances

open Draw.Commons
open Grid
open Colors

type Distances = {
    Root: Cell;
    Cells: Map<Cell, int>;

    } with

    static member ForRoot root (grid:Grid<_>) =

        let rec processFrontier frontier distances =

            match frontier with
            | [] -> distances
            | _ ->

                let visitedCells = frontier |> List.fold (fun state cell -> state |> List.append (grid.LinksOf cell |> List.where (not << distances.Cells.ContainsKey) |> List.map (fun c -> (c, match distances.[cell] with | Some x -> x + 1 | None -> -1)))) []
                let newFrontier = visitedCells |> List.map fst
                let newDistances = {distances with Cells = distances.Cells |> Map.toList |> List.append visitedCells |> Map.ofList }
                processFrontier newFrontier newDistances

        {Root = root; Cells = [root, 0] |> Map.ofList} |> processFrontier [root]

    member this.Item cell : int Option = this.Cells.TryFind cell

    member this.Set cell distance = {this with Cells = this.Cells |> Map.add cell distance}

let max dist = dist.Cells |> Map.toList |> List.maxBy snd

type Distances with
    
    member this.Max = max this

    member this.NormItem cell : float Option =
        match this.Item cell with
        | Some dist -> Some ((dist |> float) / (snd this.Max |> float))
        | None -> None


let distancesContent (distances:Distances) cell =
    if distances.Cells.ContainsKey cell then (string distances.[cell]).PadRight 3 else "   "

let floatRgba32 (r:float, g:float, b:float) = {R = r |> byte; G = g |> byte; B = b |> byte; A = byte 255}

let emptyColor = (200.0, 200.0, 200.0) |> floatRgba32

let shadeColor (dist:Distances) cell =
    match dist.NormItem cell with
    | None -> emptyColor
    | Some n ->
        let shade = 255.0 - n * 255.0
        (shade, shade, 255.0) |> floatRgba32

let rainbowShadeWithShift (dist:Distances) cycle shift cell =
    match dist.Item cell with
    | None -> emptyColor
    | Some d ->
        let t =
            match ((d + shift |> float) % cycle) / cycle with
            | x when x < 0.5 -> x * 2.0
            | x -> 1.0 - ((x - 0.5) * 2.0)
        let hue = 360.0 * t
        let (r,g,b) = hsv2rgb (hue, 1.0, 1.0)
        (r * 255.0, g * 255.0, b * 255.0) |> floatRgba32

let rainbowShade (dist:Distances) cycle cell = rainbowShadeWithShift dist cycle 0 cell
