module Program

open System
open GameLogic

let printColorMessage color message =
    match color with
    | Red -> Console.ForegroundColor <- ConsoleColor.Red
    | Yellow -> Console.ForegroundColor <- ConsoleColor.Yellow
    | Green -> Console.ForegroundColor <- ConsoleColor.Green
    | Blue -> Console.ForegroundColor <- ConsoleColor.Blue
    | _ -> Console.ForegroundColor <- ConsoleColor.White

    printf "%s" message
    Console.ResetColor()

let printBoard (board: Board) (currentPlayer: Color) (selectedCol: int) =
    // display ball above(maybe not display if selectedCol < 0)
    for col in 0 .. (Array2D.length2 board - 1) do
        if col = selectedCol then
            printColorMessage currentPlayer "● "
        else
            printf "  "

        Console.ResetColor()

    printfn ""

    // display main table
    for row in 0 .. (Array2D.length1 board - 1) do
        for col in 0 .. (Array2D.length2 board - 1) do
            match board.[row, col] with
            | Red -> printColorMessage Red "● "
            | Yellow -> printColorMessage Yellow "● "
            | Green -> printColorMessage Green "● "
            | Blue -> printColorMessage Blue "● "
            | Empty ->
                Console.ForegroundColor <- ConsoleColor.White
                printf "○ "

        Console.ResetColor()
        printfn ""

    printfn "%s" (String.Join(" ", [| for i in 1 .. (Array2D.length2 board) -> string i |]))

let selectColumn (currentPlayer: Color) (board: Board) =
    let rec loop col =
        Console.Clear()
        printBoard board currentPlayer col
        let key = Console.ReadKey(true).Key

        match key with
        | ConsoleKey.LeftArrow -> loop (max 0 (col - 1))
        | ConsoleKey.RightArrow -> loop (min (Array2D.length2 board - 1) (col + 1))
        | ConsoleKey.Enter -> col
        | _ -> loop col

    loop 0

let chooseColor playerNumber otherPlayerColor =
    let rec loop () =
        printfn "Выберите цвет для игрока %d:" playerNumber
        printfn "1. Красный"
        printfn "2. Желтый"
        printfn "3. Зеленый"
        printfn "4. Синий"
        let choice = Console.ReadLine() |> int

        let selectedColor =
            match choice with
            | 1 -> Red
            | 2 -> Yellow
            | 3 -> Green
            | 4 -> Blue
            | _ ->
                printfn "Некорректный выбор, по умолчанию выбран Красный."
                Red

        if selectedColor = otherPlayerColor then
            printfn "Цвет уже выбран другим игроком. Пожалуйста, выберите другой цвет."
            loop ()
        else
            selectedColor

    loop ()

let rec gameLoop board player1Turn (player1Color: Color) (player2Color: Color) =
    Console.Clear()
    let currentPlayer = if player1Turn then player1Color else player2Color
    printBoard board currentPlayer -1

    if isBoardFull board then
        printfn "Ничья!"
        printfn "Нажмите Enter, чтобы вернуться в меню..."
        Console.ReadLine() |> ignore
        ()
    else if player1Turn then
        printColorMessage player1Color $"Ход игрока 1 ({player1Color})\n"
        let col = selectColumn player1Color board

        if dropBall board col player1Color then
            if checkWin board player1Color then
                printBoard board currentPlayer -1
                printColorMessage player1Color $"Игрок 1 ({player1Color}) выиграл!\n"
                printfn "Нажмите Enter, чтобы вернуться в меню..."
                Console.ReadLine() |> ignore
                ()
            else
                gameLoop board (not player1Turn) player1Color player2Color
        else
            printfn "Столбец %d заполнен, выберите другой." (col + 1)
            gameLoop board player1Turn player1Color player2Color // Ход не передается
    else
        printColorMessage player2Color $"Ход игрока 2 ({player2Color})\n"
        let col = selectColumn player2Color board

        if dropBall board col player2Color then
            if checkWin board player2Color then
                printBoard board currentPlayer -1
                printColorMessage player2Color $"Игрок 2 ({player2Color}) выиграл!\n"
                printfn "Нажмите Enter, чтобы вернуться в меню..."
                Console.ReadLine() |> ignore
                ()
            else
                gameLoop board (not player1Turn) player1Color player2Color
        else
            printfn "Столбец %d заполнен, выберите другой." (col + 1)
            gameLoop board player1Turn player1Color player2Color // Ход не передается

let rec gameLoopBot board player1Turn (player1Color: Color) (botColor: Color) =
    Console.Clear()
    let currentPlayer = if player1Turn then player1Color else botColor
    printBoard board currentPlayer -1

    if isBoardFull board then
        printfn "Ничья!"
        printfn "Нажмите Enter, чтобы вернуться в меню..."
        Console.ReadLine() |> ignore
        ()
    else if player1Turn then
        printColorMessage player1Color $"Ход игрока 1 ({player1Color})\n"
        let col = selectColumn player1Color board

        if dropBall board col player1Color then
            if checkWin board player1Color then
                printBoard board currentPlayer -1
                printColorMessage player1Color $"Игрок 1 ({player1Color}) выиграл!\n"
                printfn "Нажмите Enter, чтобы вернуться в меню..."
                Console.ReadLine() |> ignore
                ()
            else
                gameLoopBot board (not player1Turn) player1Color botColor
        else
            printfn "Столбец %d заполнен, выберите другой." (col + 1)
            gameLoopBot board player1Turn player1Color botColor
    else
        printColorMessage botColor $"Ход бота ({botColor})\n"
        botMove board botColor player1Color

        if checkWin board botColor then
            printBoard board currentPlayer -1
            printColorMessage botColor $"Бот ({botColor}) выиграл!\n"
            printfn "Нажмите Enter, чтобы вернуться в меню..."
            Console.ReadLine() |> ignore
            ()
        else
            gameLoopBot board (not player1Turn) player1Color botColor

let startGame rows cols =
    let mutable player1Color = Red
    let mutable player2Color = Yellow

    let rec mainMenu () =
        Console.Clear()
        printfn "Выберите режим игры:"
        printfn "1. Игра против второго игрока"
        printfn "2. Игра против бота"
        printColorMessage player1Color $"3. Выбор цвета первого игрока\n"
        printColorMessage player2Color $"4. Выбор цвета второго игрока\n"
        printfn "5. Выход"
        let choice = Console.ReadLine() |> int

        match choice with
        | 1 ->
            let board = createBoard rows cols
            gameLoop board true player1Color player2Color
            mainMenu ()
        | 2 ->
            let board = createBoard rows cols
            gameLoopBot board true player1Color player2Color
            mainMenu ()
        | 3 ->
            player1Color <- chooseColor 1 player2Color
            mainMenu ()
        | 4 ->
            player2Color <- chooseColor 2 player1Color
            mainMenu ()
        | 5 ->
            printfn "Выход из игры."
            Environment.Exit(0)
        | _ ->
            printfn "Некорректный выбор, попробуйте снова."
            mainMenu ()

    mainMenu ()


[<EntryPoint>]
let main argv =
    // base sizes
    let defaultRows = 6
    let defaultCols = 7

    let rows, cols =
        match argv with
        | [| rowsStr; colsStr |] ->
            match Int32.TryParse(rowsStr), Int32.TryParse(colsStr) with
            | (true, rows), (true, cols) -> rows, cols
            | _ ->
                printfn "Некорректные аргументы. Используются стандартные размеры поля: 6x7."
                defaultRows, defaultCols
        | _ ->
            printfn "Используются стандартные размеры поля: 6x7."
            defaultRows, defaultCols

    startGame rows cols
    0
