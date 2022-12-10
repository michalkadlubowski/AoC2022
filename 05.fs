module day5

open utils
open System.Collections.Generic
open System.Text.RegularExpressions

type Move = { Count: int; From: int; To: int }
type Stack = string

let mapLines (lines: string seq) =
    let firstPart =
        lines |> Seq.takeWhile (fun x -> x.StartsWith(" 1   2") = false) |> Seq.toArray

    let secondPart = lines |> Seq.skip (firstPart.Length + 2) |> Seq.toArray

    let firstPartTransposed =
        firstPart
        |> Seq.map (fun x -> "   " + x)
        |> Seq.map (fun x ->
            x.ToCharArray()
            |> Seq.mapi (fun i x -> (i, x))
            |> Seq.filter (fun x -> fst x <> 0 && fst x % 4 = 0)
            |> Seq.map snd
            |> Seq.toArray)
        |> Seq.toArray

    let stacks =
        [ 0 .. firstPartTransposed.[0].Length - 1 ]
        |> Seq.map (fun i ->
            firstPartTransposed
            |> (Seq.fold (fun state element -> state + string element.[i]) ""))
        |> Seq.toArray
        |> Seq.map (fun x -> x.TrimStart().TrimEnd())
        |> Seq.toArray

    let rx = Regex(@"move (\d+) from (\d+) to (\d+)", RegexOptions.Compiled)

    let moves =
        secondPart
        |> Seq.map (fun x -> rx.Match(x))
        |> Seq.map (fun m ->
            { Count = int m.Groups[1].Value
              From = int m.Groups[2].Value
              To = int m.Groups[3].Value })
        |> Seq.toArray

    (stacks, moves)

let answer1 () =
    let (stacks, moves) = readLines ("data\\05.txt") |> mapLines

    moves
    |> Seq.iter (fun m ->
        let toAdd = stacks.[m.From - 1].Substring(0, m.Count).ToCharArray()
        let newFrom = stacks.[m.From - 1].Substring(m.Count)
        Array.set stacks (m.From - 1) newFrom
        let partToAdd = System.String(Array.rev toAdd)
        let newVal = partToAdd + stacks.[m.To - 1]
        Array.set stacks (m.To - 1) newVal)

    let firstChars = stacks |> Array.map (fun x -> x.[0])
    System.String(firstChars)

let answer2 () =
    let (stacks, moves) = readLines ("data\\05.txt") |> mapLines

    moves
    |> Seq.iter (fun m ->
        let toAdd = stacks.[m.From - 1].Substring(0, m.Count).ToCharArray()
        let newFrom = stacks.[m.From - 1].Substring(m.Count)
        Array.set stacks (m.From - 1) newFrom
        let partToAdd = System.String(toAdd)
        let newVal = partToAdd + stacks.[m.To - 1]
        Array.set stacks (m.To - 1) newVal)

    let firstChars = stacks |> Array.map (fun x -> x.[0])
    System.String(firstChars)
