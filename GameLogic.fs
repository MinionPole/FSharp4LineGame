module GameLogic

open System

type Color =
    | Red
    | Yellow
    | Green
    | Blue
    | Empty

type Board = Color[,]

let createBoard rows cols = Array2D.create rows cols Empty

let checkWin (board: Board) color =
    let rec checkDirection x y dx dy count =
        if
            x < 0
            || x >= Array2D.length1 board
            || y < 0
            || y >= Array2D.length2 board
        then
            count
        else if board.[x, y] = color then
            checkDirection (x + dx) (y + dy) dx dy (count + 1)
        else
            count

    let rec checkAllDirections x y =
        checkDirection x y 1 0 0 >= 4
        || // right
        checkDirection x y 0 1 0 >= 4
        || // down
        checkDirection x y 1 1 0 >= 4
        || // down + right
        checkDirection x y 1 -1 0 >= 4 // down + left

    seq {
        for row in 0 .. (Array2D.length1 board - 1) do
            for col in 0 .. (Array2D.length2 board - 1) do
                if board.[row, col] = color && checkAllDirections row col then
                    yield true
    }
    |> Seq.exists (id)

let isBoardFull (board: Board) =
    seq {
        for col in 0 .. (Array2D.length2 board - 1) do
            if board.[0, col] = Empty then
                yield false
    }
    |> Seq.forall (id)

let dropBall (board: Board) col color =
    if board.[0, col] <> Empty then
        false
    else
        let rec drop row =
            if row = (Array2D.length1 board - 1) || board.[row + 1, col] <> Empty then
                board.[row, col] <- color
            else
                drop (row + 1)

        drop 0
        true

let botMove (board: Board) (botColor: Color) (playerColor: Color) =
    let rnd = Random()

    let canWin color =
        seq {
            for col in 0 .. (Array2D.length2 board - 1) do
                let boardCopy = Array2D.copy board

                if dropBall boardCopy col color then
                    if checkWin boardCopy color then
                        yield Some col
                else
                    yield None
        }
        |> Seq.tryPick id

    let canBlock () = canWin playerColor

    match canWin botColor with
    | Some col ->
        dropBall board col botColor |> ignore
        printfn "Бот выбрал колонку %d" (col + 1)
    | None ->
        match canBlock () with
        | Some col ->
            dropBall board col botColor |> ignore
            printfn "Бот выбрал колонку %d" (col + 1)
        | None ->
            let rec chooseRandomCol () =
                let col = rnd.Next(Array2D.length2 board)

                if dropBall board col botColor then
                    printfn "Бот выбрал колонку %d" (col + 1)
                else
                    chooseRandomCol ()

            chooseRandomCol ()
