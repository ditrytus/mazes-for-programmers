namespace Mazes.Core.Draw

module Commons =
    
    open Mazes.Core.Grid
    
    type PointF = {X:float32; Y:float32} with
        static member (+) (a:PointF, b:PointF) =
            {X=a.X+b.X; Y=a.Y+b.Y}
        static member (-) (a:PointF, b:PointF) =
            {X=a.X-b.X; Y=a.Y-b.Y}
            
    type Size2DInt = {Width:int; Height:int}
    
    type Rgba32 = {R:byte; G:byte; B:byte; A:byte} with
        static member White:Rgba32 = {R=byte 255; G=byte 255; B=byte 255; A=byte 255}
    
    type CellDrawData = {BackgroundColor:Rgba32; Polygon:PointF list; Walls:PointF list list}
    
    type GridDrawData = {Size:Size2DInt; CellsDrawData:CellDrawData list}
    
    type RenderingEngine = GridDrawData -> string -> unit
    
    let drawRegular (engine:RenderingEngine) (filename:string) cellSize (cellColor:Cell->Rgba32) (grid:RegularGrid) =
        let drawData cell =
            let {Row=row; Column=col} = cell
            
            let cellSizeF = float32 cellSize
            let xf, yf = float32 row, float32 col

            let nwCorner = {X=yf * cellSizeF; Y=xf * cellSizeF}
            let neCorner = {X=(yf + 1.0f) * cellSizeF; Y=xf * cellSizeF}
            let swCorner = {X=yf * cellSizeF; Y=(xf + 1.0f) * cellSizeF}
            let seCorner = {X=(yf + 1.0f) * cellSizeF; Y=(xf + 1.0f) * cellSizeF}
            
            let halfV = {X=0.0f; Y=0.5f}
            let halfH = {X=0.5f; Y=0.0f}
            
            let sides = [
                        North, [nwCorner + halfV; neCorner + halfV];
                        South, [swCorner - halfV; seCorner - halfV];
                        East, [neCorner - halfH; seCorner - halfH ];
                        West, [nwCorner + halfH; swCorner + halfH]
                        ]
                        |> Map.ofList
            
            {
                BackgroundColor=cellColor cell;
                Polygon=[nwCorner; neCorner; seCorner; swCorner];
                Walls = RegularDirection.All
                            |> List.where (fun dir -> grid.IsLinkedTo dir cell |> not)
                            |> List.map (fun dir -> sides.[dir])
            }
        
        let gridDrawData =
            {
                Size = {Width=grid.ColumnsCount * cellSize; Height=grid.RowsCount * cellSize};
                CellsDrawData = grid.Cells |> List.map drawData;
            }
            
        engine gridDrawData filename