module day6

open utils

let findPacketStart len =
    let startIndex =
        (readText "data\\06.txt").ToCharArray()
        |> Seq.windowed len
        |> Seq.findIndex (fun x -> x |> Seq.distinct |> Seq.length = len)

    startIndex + len

let answer1 () = findPacketStart 4

let answer2 () = findPacketStart 14
