module DrawPngPolar

open System
open Grid
open SixLabors.Primitives
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Processing.Drawing
open SixLabors.ImageSharp.Processing.Drawing.Pens

let drawPngPolar filename cellSize (cellColor:Cell->Rgba32) (grid:PolarGrid) =
    let imageSize = 2 * grid.RowsCount * cellSize
    use bitmap = new Image<Rgba32>(imageSize + 1, imageSize + 1)
    let center = float32 imageSize / 2.0f
    let drawCell cell =
        let {Row=row; Column=col} = cell
        
        let theta = 2.0f * System.MathF.PI / (float32 grid.Rows.[row].Length)
        let innerRadius = row * cellSize |> float32
        let outerRadius = (row + 1) * cellSize |> float32
        let thetaCCW = float32 col * theta
        let thetaCW = (float32 col + 1.0f) * theta
        
        let a = PointF ((MathF.Cos thetaCCW) * innerRadius + center, (MathF.Sin thetaCCW) * innerRadius + center)
        let b = PointF ((MathF.Cos thetaCCW) * outerRadius + center, (MathF.Sin thetaCCW) * outerRadius + center)
        let c = PointF ((MathF.Cos thetaCW) * innerRadius + center, (MathF.Sin thetaCW) * innerRadius + center)
        let d = PointF ((MathF.Cos thetaCW) * outerRadius + center, (MathF.Sin thetaCW) * outerRadius + center)
            
        let walls = [
                    In, [| a; c |];
                    Out, [| b; d |];
                    Clockwise, [| c; d |];
                    CounterClockwise, [| a; b |]
                    ]
                    |> Map.ofList

        let linesToDraw = PolarDirection.All
                        |> List.where (fun dir -> grid.IsLinkedTo dir {Row=row; Column=col} |> not)
                        |> List.map (fun dir -> walls.[dir])
        
        let drawRectangle (ctx:IImageProcessingContext<Rgba32>) =
            ctx.FillPolygon (
                Brushes.Brushes.Solid (cellColor cell),
                [|a; b; d; c|])

        let drawLines (ctx:IImageProcessingContext<Rgba32>) =
            linesToDraw 
            |> List.fold (fun (ctx:IImageProcessingContext<Rgba32>) points ->
                ctx.DrawLines(Pens.Solid (Rgba32.Black, 1.0f), points)) ctx

        bitmap.Mutate (drawRectangle >> drawLines >> ignore)

    grid.Cells |> Seq.iter drawCell
    bitmap.Save filename

let colorBackground color _ = color

let whiteBackground cell = colorBackground Rgba32.White cell

let drawWhitePngPolar filename cellSize (grid:PolarGrid) = drawPngPolar filename cellSize whiteBackground grid

