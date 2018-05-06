module Grid

module Range =
    let contains x (min, max) = min <= x && x <= max

type Cell = int * int

let constrain width height (row, column) =
    let horizontal = (0, (width-1))
    let vertical = (0, (height-1))
    if (vertical |> Range.contains row) && (horizontal |> Range.contains column) then Some (row, column) else None

type Link = Cell * Cell

let areEqual (a,b) (c,d) = (a,b) = (c,d) || (a,b) = (d,c) || (b,a) = (c,d) || (b,a) = (d,c)

type Direction = West | South | East | North with
    static member All = [West; South; East; North]

type Neighbours = Map<Cell,Map<Direction,Option<Cell>>>

let toChooser e condition = if condition then Some e else None

type Grid =
    {
        Cells: Cell list;
        Links: Link list;
        Neighbourhood: Neighbours;
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

type Grid with
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

let generateCellList rows columns = [for i in 0..rows-1 do for j in 0..columns-1 -> i, j]

let prepareGrid rows columns = 
    let cells = generateCellList rows columns
    let constrain = constrain columns rows
    let neighbours = cells 
                    |> List.map (
                        fun (row, col) -> (row, col),
                                            [
                                                North, constrain (row-1, col);
                                                South, constrain (row+1, col);
                                                East, constrain (row, col+1);
                                                West, constrain (row, col-1)
                                            ] |> Map.ofList)
                    |> Map.ofList
    {Cells = cells; Links = []; Neighbourhood = neighbours; ColumnsCount = columns; RowsCount = rows; }