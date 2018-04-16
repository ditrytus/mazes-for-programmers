module DrawPng

open Grid
open SixLabors.Primitives
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Processing.Drawing
open SixLabors.ImageSharp.Processing.Drawing.Pens

let drawPng filename cellSize (cellColor:Cell->Rgba32) (grid:Grid) =
    use bitmap = new Image<Rgba32>(grid.ColumnsCount * cellSize, grid.RowsCount * cellSize)
    let drawCell (x, y) =

        let cellSizeF = float32 cellSize
        let xf, yf = float32 x, float32 y

        let nwCorner = PointF (yf * cellSizeF, xf * cellSizeF)
        let neCorner = PointF ((yf + 1.0f) * cellSizeF, xf * cellSizeF)
        let swCorner = PointF (yf * cellSizeF, (xf + 1.0f) * cellSizeF)
        let seCorner = PointF ((yf + 1.0f) * cellSizeF, (xf + 1.0f) * cellSizeF)

        let halfV = PointF (0.0f, 0.5f)
        let halfH = PointF (0.5f, 0.0f)
        let walls = [
                    North, [| nwCorner + halfV; neCorner + halfV |];
                    South, [| swCorner - halfV; seCorner - halfV |];
                    East, [| neCorner - halfH; seCorner - halfH |];
                    West, [| nwCorner + halfH; swCorner + halfH |]
                    ]
                    |> Map.ofList

        let linesToDraw = Direction.All
                        |> List.where (fun dir -> grid.IsLinkedTo dir (x, y) |> not)
                        |> List.map (fun dir -> walls.[dir])
        
        let drawRectangle (ctx:IImageProcessingContext<Rgba32>) =
            ctx.FillPolygon (
                Brushes.Brushes.Solid (cellColor (x, y)),
                [|nwCorner; neCorner; seCorner; swCorner|])

        let drawLines (ctx:IImageProcessingContext<Rgba32>) =
            linesToDraw 
            |> List.fold (fun (ctx:IImageProcessingContext<Rgba32>) points ->
                ctx.DrawLines(Pens.Solid (Rgba32.Black, 1.0f), points)) ctx

        bitmap.Mutate (fun ctx -> ctx |> drawRectangle |> drawLines |> ignore)

    grid.Cells |> Seq.iter drawCell
    bitmap.Save filename

let colorBackground color cell = color

let whiteBackground cell = colorBackground Rgba32.White cell

let drawWhitePng filename cellSize (grid:Grid) = drawPng filename cellSize whiteBackground grid