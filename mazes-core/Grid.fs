namespace Mazes.Core

module Grid =

    module Range =
        let contains x (min, max) = min <= x && x <= max

    type Cell = {Row:int; Column:int}

    let row cell = cell.Row

    let column cell = cell.Column

    let toCell (a, b) = {Row=a; Column=b}

    let makeCell row column = {Row=row; Column=column}

    type Link = Cell * Cell

    let areEqual (a,b) (c,d) = (a,b) = (c,d) || (a,b) = (d,c) || (b,a) = (c,d) || (b,a) = (d,c)

    type PolarDirection = Clockwise | CounterClockwise | In | Out with
        static member All = [Clockwise; CounterClockwise; In; Out]

    type RegularDirection = West | South | East | North with
        static member All = [West; South; East; North]

    type Neighbours<'d when 'd : comparison> = Map<Cell,Map<'d,Option<Cell>>>

    let toChooser e condition = if condition then Some e else None

    type Grid<'d when 'd : comparison> =
        {
            Cells: Cell list;
            Links: Link list;
            Neighbourhood: Neighbours<'d>;
            ColumnsCount: int;
            RowsCount: int;
        } with

        member this.Link cellA cellB =
            {this with Links = (cellA, cellB) :: this.Links}

        member this.Unlink cellA cellB =
            {
                this with
                     Links = this.Links
                             |> List.choose (fun link -> areEqual (cellA, cellB) link |> toChooser link)
            }

        member this.Size = this.Cells.Length

        member this.RandomCell = 
            let rnd = System.Random()
            this.Cells |> List.item (rnd.Next <| this.Size)

    let rows cells = cells  |> List.groupBy row |> List.map snd

    type Grid<'d when 'd : comparison> with
        member this.Rows = this.Cells |> rows

        member this.GoTo dir cell =
            match this.Neighbourhood.TryFind cell with
            | Some n -> n.[dir]
            | None -> None

        member this.AreLinked cellA cellB = this.Links |> List.exists (fun link -> areEqual link (cellA, cellB))

        member this.IsLinkedTo dir cell =
            match this.GoTo dir cell with
            | Some neighbour -> this.AreLinked cell neighbour
            | None -> false

        member this.NeighboursOf cell = this.Neighbourhood.[cell] |> Map.toList |> List.map snd |> List.choose id

        member this.LinksOf cell = this.NeighboursOf cell |> List.where (this.AreLinked cell)

        member this.DeadEnds = this.Cells |> List.where (fun cell -> (this.LinksOf cell |> List.length) = 1)
            
    type RegularGrid = Grid<RegularDirection>
    type PolarGrid = Grid<PolarDirection>

    let generateCellList rows columns = [for i in 0..rows-1 do for j in 0..columns-1 -> (i, j) |> toCell]

    let constrain width height cell =
        let horizontal = (0, (width-1))
        let vertical = (0, (height-1))
        if (vertical |> Range.contains cell.Row) && (horizontal |> Range.contains cell.Column) then Some cell else None
        
    let prepareGrid neighboursForCell rows columns  : Grid<'d> =
        let cells = generateCellList rows columns    
        let neighbours = cells |> List.map neighboursForCell |> Map.ofList
        {Cells = cells; Links = []; Neighbourhood = neighbours; ColumnsCount = columns; RowsCount = rows; }

    let prepareRegularGrid rows columns : RegularGrid =
        let neighboursForCell = fun cell ->
            let constrain = constrain columns rows
            cell,
            [
                North, constrain {cell with Row = cell.Row-1};//(row-1, col);
                South, constrain {cell with Row = cell.Row+1};//(row+1, col);
                East, constrain {cell with Column = cell.Column+1};//(row, col+1);
                West, constrain {cell with Column = cell.Column-1};//(row, col-1)
            ] |> Map.ofList
        prepareGrid neighboursForCell rows columns
        
    let preparePolarGrid rows columns : PolarGrid =
        let neighboursForCell = fun cell ->
            let constrain = constrain columns rows
            cell,
            [
                In, constrain {cell with Row = cell.Row-1};
                Out, constrain {cell with Row = cell.Row+1};
                Clockwise, constrain {cell with Column = cell.Column+1};
                CounterClockwise, constrain {cell with Column = cell.Column-1};
            ] |> Map.ofList
        prepareGrid neighboursForCell rows columns