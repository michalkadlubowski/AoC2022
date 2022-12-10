module day4

open utils
open System.Collections.Generic

type Range = int * int

let mapToRange (rangeStr: string) : Range =
    let parts = rangeStr.Split('-')
    (int parts.[0], int parts.[1])

let mapLine (line: string) =
    let parts = line.Split(',')
    (mapToRange parts.[0], mapToRange parts.[1])

let isContained (r1: Range, r2: Range) =
    (fst r1 <= fst r2 && snd r1 >= snd r2) || (fst r2 <= fst r1 && snd r2 >= snd r1)

let doOverlap (r1: Range, r2: Range) =
    not (snd r1 < fst r2 || snd r2 < fst r1)

let answer1 () =
    readLines ("data\\04.txt")
    |> Seq.map mapLine
    |> Seq.filter isContained
    |> Seq.length

let answer2 () =
    readLines ("data\\04.txt")
    |> Seq.map mapLine
    |> Seq.filter doOverlap
    |> Seq.length
