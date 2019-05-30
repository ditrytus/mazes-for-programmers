module Grid

module Range =
    let contains x (min, max) = min <= x && x <= max

type Cell = int * int

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

let rows cells = cells |> Seq.ofList |> Seq.groupBy fst |> Seq.map snd

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

let generateCellList rows columns = [for i in 0..rows-1 do for j in 0..columns-1 -> i, j]

let constrain width height (row, column) =
    let horizontal = (0, (width-1))
    let vertical = (0, (height-1))
    if (vertical |> Range.contains row) && (horizontal |> Range.contains column) then Some (row, column) else None
    
let prepareGrid neighboursForCell rows columns  : Grid<'d> =
    let cells = generateCellList rows columns    
    let neighbours = cells |> List.map neighboursForCell |> Map.ofList
    {Cells = cells; Links = []; Neighbourhood = neighbours; ColumnsCount = columns; RowsCount = rows; }

let prepareRegularGrid rows columns : RegularGrid =
    let neighboursForCell = fun (row, col) ->
        let constrain = constrain columns rows
        (row, col),
        [
            North, constrain (row-1, col);
            South, constrain (row+1, col);
            East, constrain (row, col+1);
            West, constrain (row, col-1)
        ] |> Map.ofList
    prepareGrid neighboursForCell rows columns
    
let preparePolarGrid rows columns : PolarGrid =
    let neighboursForCell = fun (row, col) ->
        let constrain = constrain columns rows
        (row, col),
        [
            In, constrain (row-1, col);
            Out, constrain (row+1, col);
            Clockwise, constrain (row, col+1);
            CounterClockwise, constrain (row, col-1)
        ] |> Map.ofList
    prepareGrid neighboursForCell rows columns