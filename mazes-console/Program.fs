open Grid
open RecursiveBacktracker
open DrawAscii

[<EntryPoint>]
let main argv =
    prepareGrid 10 10 |> recursiveBacktracker |> drawAsciiEmpty |> printfn "\n%s"
    0 // return an integer exit code
