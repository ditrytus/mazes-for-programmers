namespace Mazes.Core.Draw

module AsciiRegular =

    open Mazes.Core.Grid

    let drawAscii contentsOf (grid:RegularGrid) = 

        let drawRow (row: Cell seq) = 

            let drawMiddle (cell:Cell) =
                contentsOf cell + (if grid.IsLinkedTo East cell then " " else "║")

            let drawBottom (cell:Cell) =
                (if grid.IsLinkedTo South cell then "   " else "═══") + "╬"

            "║" + (row |> Seq.map drawMiddle |> String.concat "") + "\n" +
            "╬" + (row |> Seq.map drawBottom |> String.concat "") + "\n"

        "╬" + ("═══╬" |> String.replicate grid.ColumnsCount) + "\n" +
        (generateCellList grid.RowsCount grid.ColumnsCount |> rows |> Seq.map drawRow |> String.concat "")

    let emptyContent _ = "   "

    let drawAsciiEmpty = drawAscii emptyContent