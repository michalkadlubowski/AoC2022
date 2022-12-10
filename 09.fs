module day9

open utils

type Line = Position array

type Direction =
    | Left
    | Right
    | Up
    | Down

let updateTail head tail =
    let (hX, hY) = head
    let (tX, tY) = tail
    let xModifier = hX - tX
    let yModifier = hY - tY

    match (xModifier, yModifier) with
    | (2, 2) -> (tX + 1, tY + 1)
    | (2, 1) -> (tX + 1, tY + 1)
    | (2, 0) -> (tX + 1, tY)
    | (2, -1) -> (tX + 1, tY - 1)
    | (2, -2) -> (tX + 1, tY - 1)
    | (1, 2) -> (tX + 1, tY + 1)
    | (0, 2) -> (tX, tY + 1)
    | (-1, 2) -> (tX - 1, tY + 1)
    | (-2, 2) -> (tX - 1, tY + 1)
    | (1, -2) -> (tX + 1, tY - 1)
    | (0, -2) -> (tX, tY - 1)
    | (-1, -2) -> (tX - 1, tY - 1)
    | (-2, -2) -> (tX - 1, tY - 1)
    | (-2, 1) -> (tX - 1, tY + 1)
    | (-2, 0) -> (tX - 1, tY)
    | (-2, -1) -> (tX - 1, tY - 1)
    | _ -> (tX, tY)

let processMove (line: Position array) direction =
    let (hX, hY) = line.[0]

    match direction with
    | Direction.Left -> Array.set line 0 (hX - 1, hY)
    | Direction.Right -> Array.set line 0 (hX + 1, hY)
    | Direction.Up -> Array.set line 0 (hX, hY + 1)
    | Direction.Down -> Array.set line 0 (hX, hY - 1)

let rec getAllPositions (moves: Direction[]) (line: Line) (allVisited: Position list) =
    if moves.Length = 0 then
        allVisited
    else
        processMove line moves.[0]

        line
        |> Array.skip 1
        |> Array.iteri (fun i x -> Array.set line (i + 1) (updateTail line.[i] line.[i + 1]))

        let newVisited = (Array.last line) :: allVisited
        getAllPositions moves[1..] line newVisited

let moves () =
    readLines ("data\\09.txt")
    |> Seq.map (fun x -> x.Split(' '))
    |> Seq.map (fun x -> (x.[0], int x.[1]))
    |> Seq.map (fun (s, i) -> [ 1..i ] |> Seq.map (fun _ -> s))
    |> Seq.collect id
    |> Seq.map (fun x ->
        match x with
        | "R" -> Direction.Right
        | "L" -> Direction.Left
        | "U" -> Direction.Up
        | "D" -> Direction.Down)
    |> Seq.toArray

let answer1 () =
    let mov = moves ()
    let line = Array.init 2 (fun i -> (0, 0))
    let visitedByTail = getAllPositions mov line List.empty
    visitedByTail |> Seq.distinct |> Seq.length

let answer2 () =
    let mov = moves ()
    let line = Array.init 10 (fun i -> (0, 0))
    let visitedByTail = getAllPositions mov line List.empty
    visitedByTail |> Seq.distinct |> Seq.length
