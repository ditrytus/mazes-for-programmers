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

type Direction = West | South | East | North

let allDirections = [West; South; East; North]

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

    member this.Rows = this.Cells |> Seq.ofList |> Seq.groupBy (fun (r, _) -> r) |> Seq.map (fun (_,g) -> g)

    member this.GoTo dir cell = this.Neighbourhood.[cell].[dir]

    member this.AreLinked cellA cellB = this.Links |> List.exists (fun link -> areEqual link (cellA, cellB))

    member this.IsLinkedTo dir cell =
        match this.GoTo dir cell with
        | Some neighbour -> this.AreLinked cell neighbour
        | None -> false

let prepareGrid rows columns = 
    let cells = [for i in 0..rows-1 do for j in 0..columns-1 -> i, j]
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