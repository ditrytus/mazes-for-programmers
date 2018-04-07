module DrawAscii

open Grid
let drawAscii (grid:Grid) = 

    let drawRow (row: Cell seq) = 

        let drawMiddle (cell:Cell) =
            "   " + (if grid.IsLinkedTo East cell then " " else "║")

        let drawBottom (cell:Cell) =
            (if grid.IsLinkedTo South cell then "   " else "═══") + "╬"

        "║" + (row |> Seq.map drawMiddle |> String.concat "") + "\n" +
        "╬" + (row |> Seq.map drawBottom |> String.concat "") + "\n"

    "╬" + ("═══╬" |> String.replicate grid.ColumnsCount) + "\n" +
    (grid.Rows |> Seq.map drawRow |> String.concat "")
    