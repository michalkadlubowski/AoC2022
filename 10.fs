module day10

open utils

type Instruction = int list -> int list

let mapLineToInstruction =
    function
    | "noop" -> (fun (li: int list) -> li @ [ (li.[li.Length - 1]) ])
    | x ->
        (fun (li: int list) ->
            let last = List.last li
            let toAdd = int (x.Split(' ').[1])
            let newVal = last + toAdd
            let l = List.append li [ last ]
            List.append l [ newVal ])

let isSpriteWithinPixel (registers: int array) cycle drawPosition =
    let xVal = registers.[cycle]
    if abs (xVal - drawPosition) < 2 then true else false

let drawLine (line: bool seq) =
    line
    |> Seq.map (fun x -> if x then '#' else '.')
    |> Seq.iter (fun x -> printf "%c" x)

    printfn ""

let answer1 () =
    let registers =
        readLines ("data\\10.txt")
        |> Seq.map mapLineToInstruction
        |> Seq.fold (fun st x -> x st) [ 1 ]
        |> Seq.toArray

    let signaPos = [| 20; 60; 100; 140; 180; 220 |]
    signaPos |> Seq.map (fun x -> x * registers.[x - 1]) |> Seq.sum


let answer2 () =
    let registers =
        readLines ("data\\10.txt")
        |> Seq.map mapLineToInstruction
        |> Seq.fold (fun st x -> x st) [ 1 ]
        |> Seq.toArray

    let shouldDraw = isSpriteWithinPixel registers

    let lines =
        [ 0..240 ]
        |> Seq.chunkBySize 40
        |> Seq.map (fun line ->
            line
            |> Seq.map (fun c ->
                let position = c % 40
                shouldDraw c position))

    lines |> Seq.iter drawLine
    0
