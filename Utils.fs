module Utils

let randomItem (l: 'a list) =
    match l with
    | [] -> None
    | l -> Some l.[(System.Random ()).Next(l.Length)]