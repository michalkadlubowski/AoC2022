module day1

open utils

let answer1 () =
    readLines ("data\\01.txt")
    |> splitSeq ("")
    |> Seq.map (Seq.map int)
    |> Seq.map (Seq.sum)
    |> Seq.max

let answer2 () =
    readLines ("data\\01.txt")
    |> splitSeq ("")
    |> Seq.map (Seq.map int)
    |> Seq.map (Seq.sum)
    |> Seq.sortByDescending id
    |> Seq.take 3
    |> Seq.sum
