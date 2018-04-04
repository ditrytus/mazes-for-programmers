module Utils

let random (l: 'a list) =
    let rand = System.Random ()
    match l with
    | [] -> None
    | l -> Some l.[rand.Next(l.Length)]