module day7

open utils


let rec processDir currentDir (dirSizes: Map<string, bigint>) (lines: string array) =
    if lines.Length = 0 then
        dirSizes
    else
        let currentLine = lines.[0]

        if currentLine = "$ cd /" then
            processDir ("/") dirSizes lines.[1..]
        elif currentLine = "$ cd .." then
            let index = currentDir.Substring(0, currentDir.Length - 1).LastIndexOf('/')
            let dirUp = currentDir.Substring(0, index) + "/"
            processDir dirUp dirSizes lines.[1..]
        elif currentLine.StartsWith("$ cd") then
            processDir (currentDir + currentLine.Substring(5) + "/") dirSizes lines.[1..]
        elif currentLine.StartsWith("$ ls") then
            processDir currentDir dirSizes lines.[1..]
        elif currentLine.StartsWith("dir") then
            let dirName = currentDir + currentLine.Substring(4) + "/"

            let newMap =
                dirSizes
                |> Map.change dirName (fun x ->
                    match x with
                    | Some s -> Some s
                    | None -> Some 0l)

            processDir currentDir newMap lines.[1..]
        else
            let size = currentLine.Split(' ').[0] |> bigint.Parse

            let newMap =
                dirSizes
                |> Map.change currentDir (fun x ->
                    match x with
                    | Some s -> Some(s + size)
                    | None -> Some size)

            processDir currentDir newMap lines.[1..]

let sumAllContaining (dir: string) (map: Map<string, bigint>) =
    map
    |> Map.toSeq
    |> Seq.filter (fun (d, s) -> d.StartsWith(dir))
    |> Seq.sumBy snd

let answer1 () =
    let map =
        readLines ("data\\07.txt")
        |> Seq.toArray
        |> processDir "" Map.empty<string, bigint>

    let mapUpdated = map |> Map.map (fun k v -> sumAllContaining k map)

    mapUpdated
    |> Map.filter (fun k v -> v <= bigint 100000)
    |> Map.values
    |> Seq.sum


let answer2 () =
    let map =
        readLines ("data\\07.txt")
        |> Seq.toArray
        |> processDir "" Map.empty<string, bigint>
        |> Map.add "/" (bigint 0)

    let mapUpdated = map |> Map.map (fun k v -> sumAllContaining k map)
    let sizeRequired = bigint 30000000
    let totalDiskSize = bigint 70000000
    let totalSizeUsed = mapUpdated.["/"]
    let minToDelete = sizeRequired - (totalDiskSize - totalSizeUsed)

    mapUpdated
    |> Map.toSeq
    |> Seq.filter (fun (d, s) -> s >= minToDelete)
    |> Seq.minBy snd
