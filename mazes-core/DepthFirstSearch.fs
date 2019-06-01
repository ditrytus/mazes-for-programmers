module DepthFirstSearch

open Grid

let findPath (start:Cell) (goal:Cell) (grid:Grid<_>) =

    let rec findPathRec (stack):Cell list Option =

        match stack  with
        | [] -> failwith "Stack should never be empty!"
        | head::tail ->

            match head with
            | h when h = goal -> head::tail |> Some
            | _ ->
                
                match head |> grid.LinksOf |> List.except (Seq.ofList tail) with
                | [] -> None
                | links ->

                    let linksFold result link = 
                        match result with
                        | None -> findPathRec (link::stack)
                        | path -> path 

                    links |> List.fold linksFold None

    match findPathRec [start] with
    | None -> []
    | Some path -> path