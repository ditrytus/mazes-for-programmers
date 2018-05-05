module MaskPng

open System.IO
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats

let fromPngFile filename  =
    let image = File.ReadAllBytes filename |> Image.Load
    Array2D.init image.Width image.Height (fun x y -> image.[x,y] = Rgba32.Black) |> Mask.fromArray 