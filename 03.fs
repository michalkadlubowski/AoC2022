module day3

open utils
open System.Collections.Generic

let splitLine (line: string) =
    let mid = line.Length / 2
    (line.[0 .. mid - 1], line.[mid..])

let distinctItems (itemsStr: string) =
    new HashSet<char>(itemsStr.ToCharArray())

let presentInAll (sets: HashSet<char> array) =
    let firstSet = sets.[0]

    firstSet
    |> Seq.find (fun item -> sets.[1..] |> Seq.fold (fun s c -> s && c.Contains(item)) true)

let getPriority c =
    let numVal = int c

    if numVal > 40 && numVal < 91 then
        numVal - 38
    else
        numVal - 96

let answer1 () =
    readLines ("data\\03.txt")
    |> Seq.map splitLine
    |> Seq.map (fun (l: string, r) -> [| distinctItems l; distinctItems r |])
    |> Seq.map presentInAll
    |> Seq.sumBy getPriority

let answer2 () =
    let itemSets = readLines ("data\\03.txt") |> Seq.map distinctItems

    itemSets
    |> Seq.splitInto ((Seq.length itemSets) / 3)
    |> Seq.map presentInAll
    |> Seq.sumBy getPriority
