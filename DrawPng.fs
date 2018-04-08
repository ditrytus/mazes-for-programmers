module DrawPng

open Grid
open SixLabors.Primitives
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Processing.Drawing
open SixLabors.ImageSharp.Processing.Drawing.Pens

let drawPng filename cellSize (grid:Grid) =
    use bitmap = new Image<Rgba32>(grid.ColumnsCount * cellSize, grid.RowsCount * cellSize)
    let drawCell (x, y) =

        let cellSizeF = float32 cellSize
        let xf, yf = float32 x, float32 y

        let nwCorner = PointF (yf * cellSizeF + 0.5f, xf * cellSizeF + 0.5f)
        let neCorner = PointF ((yf + 1.0f) * cellSizeF - 0.5f, xf * cellSizeF + 0.5f)
        let swCorner = PointF (yf * cellSizeF + 0.5f, (xf + 1.0f) * cellSizeF - 0.5f)
        let seCorner = PointF ((yf + 1.0f) * cellSizeF - 0.5f, (xf + 1.0f) * cellSizeF - 0.5f)

        let walls = [
                    North, [| nwCorner; neCorner |];
                    South, [| swCorner; seCorner |];
                    East, [| neCorner; seCorner |];
                    West, [| nwCorner; swCorner |]
                    ]
                    |> Map.ofList

        let linesToDraw = Direction.All
                        |> List.where (fun dir -> grid.IsLinkedTo dir (x, y) |> not)
                        |> List.map (fun dir -> walls.[dir])
        

        let blackPen = Pens.Solid (Rgba32.Black, 1.0f)

        let drawLines (ctx:IImageProcessingContext<Rgba32>) =
            linesToDraw 
            |> List.fold (fun (ctx:IImageProcessingContext<Rgba32>) points ->
                ctx.DrawLines(blackPen, points)) ctx
            |> ignore                       

        bitmap.Mutate drawLines

    grid.Cells |> Seq.iter drawCell
    bitmap.Save filename