module GenerationAlgorithms

open Grid
open Sidewinder
open AldousBroder
open Wilson
open HuntAndKill
open RecursiveBacktracker
    
type GenerationAlgorithm =
    | Sidewinder = 1
    | AldousBroder = 2
    | Wilson = 3
    | HuntAndKill = 4
    | RecursiveBacktracker = 5

let apply (algorithm:GenerationAlgorithm) : RegularGrid -> RegularGrid =
    match algorithm with
    | GenerationAlgorithm.Sidewinder -> sidewinder
    | GenerationAlgorithm.AldousBroder -> aldousBroder
    | GenerationAlgorithm.Wilson -> wilson
    | GenerationAlgorithm.HuntAndKill -> huntAndKill
    | GenerationAlgorithm.RecursiveBacktracker -> recursiveBacktracker
    | x -> failwithf "Could not find an algorithm for value %O" x