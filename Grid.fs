module Grid

type Cell = int * int

let inRange (min, max) x = min <= x && x <= max

let constrain width height (row, column) =
    let horizontal = (0, (width-1))
    let vertical = (0, (height-1))
    if (row |> inRange vertical) && (column |> inRange horizontal) then Some (row, column) else None

type Link = Cell * Cell

let areEqual (a,b) (c,d) = (a,b) = (c,d) || (a,b) = (d,c) || (b,a) = (c,d) || (b,a) = (d,c)

type Direction = West | South | East | North

let allDirections = [West; South; East; North]

type Neighbours = Map<Cell,Map<Direction,Option<Cell>>>

type Grid = {
    Cells: Cell list;
    Links: Link list;
    Neighbourhood: Neighbours;
    ColumnsCount: int;
    RowsCount: int;
}

let link (a:Cell) (b:Cell) grid = {grid with Links = (a,b) :: grid.Links}

let toChooser e condition = if condition then Some e else None

let unlink (a:Cell) (b:Cell) grid = {
    grid with
        Links = grid.Links
                |> List.choose (fun l -> areEqual (a, b) l |> toChooser l)
    }

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

let size grid = grid.Cells.Length

let randomCell grid = 
    let rnd = System.Random()
    grid.Cells |> List.item (rnd.Next <| size grid)

let rows grid = grid.Cells |> Seq.ofList |> Seq.groupBy (fun (r, _) -> r) |> Seq.map (fun (_,g) -> g)

let goTo grid dir cell = grid.Neighbourhood.[cell].[dir]

let areLinked grid cellA cellB = grid.Links |> List.exists (fun link -> areEqual link (cellA, cellB))

let isLinkedTo grid dir cell =
    match goTo grid dir cell with
    | Some neighbour -> areLinked grid cell neighbour
    | None -> false