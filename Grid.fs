module Grid

type Cell = int * int

let inRange (min, max) x = min <= x && x <= max

let constrain width height (row, column) =
    let horizontal = (0, (width-1))
    let vertical = (0, (height-1))
    if row |> inRange horizontal  && column |> inRange vertical then Some (row, column) else None

type Link = Cell * Cell

let areEqual (a,b) (c,d) = (a,b) <> (c,d) && (a,b) <> (c,d)

let toChooser e condition = if condition then Some e else None

type Direction = West | South | East | North

let allDirections = [West; South; East; North]

type Neighbours = Map<Cell,Map<Direction,Option<Cell>>>

type Grid = {
    Cells: Cell list;
    Links: Link list;
    Neighbourhood: Neighbours
}

let link (a:Cell) (b:Cell) grid = {grid with Links = (a,b) :: grid.Links}

let unlink (a:Cell) (b:Cell) grid = {
    grid with
        Links = grid.Links
                |> List.choose (fun l -> areEqual (a, b) l |> toChooser l)
    }

let prepareGrid rows columns = 
    let cells = [for i in 0..rows do for j in 0..columns -> i, j]
    let constrain = constrain rows columns
    let neighbours = cells 
                    |> List.map (
                        fun (row, col) -> (row, col),
                                            [
                                                North, constrain (row-1, col);
                                                South, constrain (row+1, col);
                                                East, constrain (row, col-1);
                                                West, constrain (row, col+1)
                                            ] |> Map.ofList)
                    |> Map.ofList
    {Cells = cells; Links = []; Neighbourhood = neighbours}

let size g = g.Cells.Length

let randomCell g = 
    let rnd = System.Random()
    g.Cells |> List.item (rnd.Next <| size g)

let rows g = g.Cells |> Seq.ofList |> Seq.groupBy (fun (r, _) -> r)

let goTo grid dir cell = grid.Neighbourhood.[cell].[dir]