open Grid
open DrawAscii
open Argu
open Arguments

[<EntryPoint>]
let main argv =
    try
        let parser = ArgumentParser.Create<MainArgs>(programName = "mazes-console")
        let args = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)

        let emptyGrid = prepareGrid (args.GetResult Height) (args.GetResult Width)
        
        let maskedGrid =
            match args.TryGetResult Mask with
            | Some mask -> emptyGrid |> Mask.mask (MaskPng.fromPngFile mask)
            | None -> emptyGrid

        maskedGrid |> GenerationAlgorithms.apply (args.GetResult Algorithm)
            |> drawAsciiEmpty
            |> printfn "\n%s"
    with e ->
        printfn "%s" e.Message
    0
