module Arguments

open Argu
open GenerationAlgorithms

(*

generate
- size
- mask
- algorithm

distances
- start point

find path
- longest (require distances)
| arbitrary
    - algorithm
        - Dijkstra (require distances)
            - end point
        - DepthFirst
            - start point
            - end point

print ascii
- cell width (default 3)
- cell content
    - empty
    - filled
        - content
            - character (default * )
            - distances numbers (requires distances)
        - fill type
            - all
            - mask with path (requires any find path)

draw png
- cell size (default 10)
- output file (default autogenerate)
    - autogenerate file name
    - explicit file name
- wall color (default black)
- wall thickness (default 1px)
- background color (default solid)
    - solid (default white)
    - shade distance (require distances)
        - shade scheme (R|G|B)
        - rainbow
            - cycles
    - mask with path (requires any path)
        - basic color (default white)

*)

type PrintAsciiArgs = 
    | [<AltCommandLine("-cw")>] CellWidth of int
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | CellWidth _ -> "Width of printed cell in characters."

type MainArgs =
    | [<Mandatory; Unique>] Height of int
    | [<Mandatory; Unique>] Width of int
    | [<Mandatory; Unique>] Algorithm of GenerationAlgorithm
    //| PrintAscii of ParseResults<PrintAsciiArgs>
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Height _ -> "Height of maze in cells."
            | Width _ -> "Width of maze in cells."
            | Algorithm _ -> "Algorithm which will be used to generate a maze."
            //| PrintAscii _ -> "Prints an ASCII art representation of a generated maze."