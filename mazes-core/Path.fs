namespace Mazes.Core

module Path =

    let pathContent path content cell = if path |> List.contains cell then content cell else "   "

    let pathContentString path str cell = cell |> pathContent path (fun _ -> str)

    let pathContentDistance path cell =
        cell |> pathContent path (fun c -> (List.findIndex (fun p -> p = c) path |> string).PadRight 3)