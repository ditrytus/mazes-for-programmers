module DrawAscii

open Grid
let drawAscii (g:Grid) = 
    let drawRow (row: Cell seq) = 
        let drawMiddle (cell:Cell) =
            "   " + (if isLinkedTo g East cell then " " else "║")
        let drawBottom (cell:Cell) =
            (if isLinkedTo g South cell then "   " else "═══") + "╬"
        "║" + (row |> Seq.map (fun cell -> drawMiddle cell) |> String.concat "") + "\n" +
        "╬" + (row |> Seq.map (fun cell -> drawBottom cell) |> String.concat "") + "\n"
    "╬" + ("═══╬" |> String.replicate g.ColumnsCount) + "\n" +
    (g |> rows |> Seq.map (fun row -> drawRow row) |> String.concat "")
    