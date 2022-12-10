module day2

open utils

type RPS =
    | Rock
    | Paper
    | Scissors

type Round = RPS * RPS

let scoreP2Round (p1Play: RPS, p2Play) =
    let p2SignScore =
        match p2Play with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3

    let p2ResultScore =
        match (p1Play, p2Play) with
        | (x, y) when x.Equals y -> 3
        | (Scissors, Rock) -> 6
        | (Rock, Paper) -> 6
        | (Paper, Scissors) -> 6
        | _ -> 0

    p2SignScore + p2ResultScore

let playMapper playChar =
    match playChar with
    | "A"
    | "X" -> RPS.Rock
    | "B"
    | "Y" -> RPS.Paper
    | "C"
    | "Z" -> RPS.Scissors
    | _ -> invalidArg "playChar" "unparsable"

let mapRoundPart1 (input: string) =
    let plays = input.Split(" ")
    (playMapper plays.[0], playMapper plays.[1])

let mapRoundPart2 (input: string) =
    let plays = input.Split(" ")
    let p1Play = playMapper plays.[0]

    let p2Play =
        match (p1Play, plays.[1]) with
        | (_ as x, "Y") -> x
        | (RPS.Rock, "X") -> RPS.Scissors
        | (RPS.Rock, "Z") -> RPS.Paper
        | (RPS.Paper, "X") -> RPS.Rock
        | (RPS.Paper, "Z") -> RPS.Scissors
        | (RPS.Scissors, "X") -> RPS.Paper
        | (RPS.Scissors, "Z") -> RPS.Rock
        | _ -> invalidArg "p2Play" "unparsable"

    (p1Play, p2Play)

let answer1 () =
    readLines ("data\\02.txt")
    |> Seq.map mapRoundPart1
    |> Seq.map scoreP2Round
    |> Seq.sum

let answer2 () =
    readLines ("data\\02.txt")
    |> Seq.map mapRoundPart2
    |> Seq.map scoreP2Round
    |> Seq.sum
