module day8

open utils

let getVisible treeSeqs =
    treeSeqs
    |> Seq.map (fun x ->
        x
        |> Seq.fold
            (fun (max, all) curr ->
                if (snd curr) > max then
                    ((snd curr), [ curr ] @ all)
                else
                    (max, all))
            (-1, [])
        |> snd)
    |> Seq.collect id
    |> Seq.toList

let getVisible2 treeSeqs limit =
    let visible =
        treeSeqs
        |> Seq.map (fun x ->
            x
            |> Seq.fold
                (fun (doLook, all) curr ->
                    if doLook && (snd curr) >= limit then
                        (false, [ curr ] @ all)
                    elif doLook && (snd curr) < limit then
                        (true, [ curr ] @ all)
                    else
                        (false, all))
                (true, [])
            |> snd)
        |> Seq.collect id
        |> Seq.toList

    visible

let answer1 () =
    let treemap: (Position * int)[,] =
        readLines ("data\\08.txt")
        |> Seq.map (fun x -> x.ToCharArray())
        |> array2D
        |> Array2D.mapi (fun x y p -> ((x, y), (charToInt p)))

    let fromTop = treemap |> array2DtoJaggedColumns
    let fromBottom = treemap |> array2DtoJaggedColumns |> Seq.map Seq.rev
    let fromLeft = treemap |> array2DtoJagged
    let fromRight = treemap |> array2DtoJagged |> Seq.map Seq.rev
    let topTrees = fromTop |> getVisible
    let bottmTrees = fromBottom |> getVisible
    let leftTrees = fromLeft |> getVisible
    let rightTrees = fromRight |> getVisible

    topTrees @ bottmTrees @ leftTrees @ rightTrees
    |> Seq.distinctBy fst
    |> Seq.length


let answer2 () =
    let treemap: (Position * int)[,] =
        readLines ("data\\08.txt")
        |> Seq.map (fun x -> x.ToCharArray())
        |> array2D
        |> Array2D.mapi (fun x y p -> ((x, y), (charToInt p)))

    treemap
    |> getAllElements
    |> Seq.map (fun (pos, height) ->
        let directionSeqs =
            transformations
            |> Seq.map (fun trans ->
                seq { 1..100 }
                |> Seq.takeWhile (fun dist ->
                    let newPos = trans dist pos
                    withinBounds newPos treemap)
                |> Seq.map (fun x -> trans x pos)
                |> Seq.map (fun (x, y) -> treemap.[x, y])
                |> Seq.toArray)
            |> Seq.toArray

        directionSeqs
        |> Seq.map (fun x -> getVisible2 [ x ] height)
        |> Seq.map Seq.length
        |> Seq.reduce (*))
    |> Seq.max
