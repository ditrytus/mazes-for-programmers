namespace Mazes.Core.Draw

module PngRegular = 

    open Mazes.Core.Grid
    open Commons
    open SixLabors.Primitives
    open SixLabors.ImageSharp
    open SixLabors.ImageSharp.PixelFormats
    open SixLabors.ImageSharp.Processing
    open SixLabors.ImageSharp.Processing.Drawing
    open SixLabors.ImageSharp.Processing.Drawing.Pens

    type ImgContext = IImageProcessingContext<Rgba32> 

    type CellDrawContext = ImgContext * CellDrawData

    let pngRenderingEngine (drawData:GridDrawData) (filename:string) =

        use bitmap = new Image<Rgba32>(drawData.Size.Width, drawData.Size.Height)

        let toImageSharpColor (color:Commons.Rgba32) = Rgba32(color.R, color.G, color.B, color.A)

        let toImageSharpPointF (point:Commons.PointF) = PointF(point.X, point.Y)
        
        let drawPolygon ((ctx,cellDrawData):CellDrawContext) =
            let newImgContext =
                ctx.FillPolygon (
                    Brushes.Brushes.Solid (cellDrawData.BackgroundColor |> toImageSharpColor),
                    cellDrawData.Polygon |> List.map toImageSharpPointF |> Array.ofList)
            (newImgContext, cellDrawData)

        let drawWalls ((ctx,cellDrawData):CellDrawContext) =
            let newImgContext = 
                cellDrawData.Walls
                    |> List.fold (fun (ctx:ImgContext) wall ->
                        ctx.DrawLines(Pens.Solid (Rgba32.Black, 1.0f), wall |> List.map toImageSharpPointF |> Array.ofList))
                        ctx
            (newImgContext, cellDrawData)      

        drawData.CellsDrawData 
        |> List.iter (fun cellDrawData ->
            bitmap.Mutate (fun imgCtx ->
                (imgCtx, cellDrawData) |> drawPolygon |> drawWalls |> ignore))

        bitmap.Save filename
        
    let drawPngRegular = drawRegular pngRenderingEngine

    let colorBackground color _ = color

    let whiteBackground cell = colorBackground Commons.Rgba32.White cell

    let drawWhitePngRegular filename cellSize (grid:RegularGrid) = drawPngRegular filename cellSize whiteBackground grid