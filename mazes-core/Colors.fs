module Colors

let hsv2rgb (h:float, s:float, v:float) =
    match v with
    | 0.0 -> (0.0, 0.0, 0.0)
    |_ ->
        let hq = h / 60.0
        let i = floor hq
        let f = hq - i
        let p = v * (1.0 - s)
        let q = v * (1.0 - (s * f))
        let t = v * (1.0 - (s * (1.0 - f)))
        match i with
        | 0.0 -> (v, t, p)
        | 1.0 -> (q, v, p)
        | 2.0 -> (p, v, t)
        | 3.0 -> (p, q, v)
        | 4.0 -> (t, p, v)
        | 5.0 -> (v, p, q)
        | 6.0 -> (v, t, p)
        | _ -> (0.0, 0.0, 0.0)