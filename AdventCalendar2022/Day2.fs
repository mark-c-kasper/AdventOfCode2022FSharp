module AdventCalendar2022.Day2

open System
open System.IO

type PointScore = PointScore of int

type Move =
    | Rock
    | Paper
    | Scissor



type MovePoint = {
    Move: Move
    PlayPoint: PointScore
}

let rockMove = {
    Move = Rock;
    PlayPoint = PointScore 1
}

let paperMove = {
    Move = Paper;
    PlayPoint = PointScore 2
}

let scissorMove = {
    Move = Scissor
    PlayPoint = PointScore 3 
}

let winPoint = PointScore 6
let drawPoint = PointScore 3
let losePoint = PointScore 0


type Strategy = {
    OpponentMove: char * MovePoint
    YourMove: char * MovePoint
}
        



let givenStrategy =
    """A Y
B X
C Z"""

let givenStrategyMapper (move: char) =
    // Win
    if move = 'A' then rockMove else if move = 'Y' then paperMove
    // Lose
    else if move = 'B' then paperMove else if move = 'X' then rockMove
    // Draw
    else if move = 'C' then scissorMove else if move = 'Z' then scissorMove
    else rockMove

let optimalStrategyMapper (move: char) =
    // Draw
    if move = 'A' then rockMove else if move = 'Y' then rockMove
    // Win
    else if move = 'B' then paperMove else if move = 'X' then scissorMove
    // Lose
    else if move = 'C' then scissorMove else if move = 'Z' then paperMove
    else rockMove

let processRow (row: string) strategyMapper =
    let move0 = row.Chars(0)
    let move2 = row.Chars(2)
    match string row with
    | null -> {OpponentMove = 'a', rockMove; YourMove = 'b', rockMove }
    | _ -> {OpponentMove = move0, strategyMapper move0; YourMove = move2, strategyMapper move2 }

let stringReader = new StringReader(givenStrategy)
let givenStrategy1 = processRow (stringReader.ReadLine()), givenStrategyMapper
let givenStrategy2 = processRow (stringReader.ReadLine()), givenStrategyMapper
let givenStrategy3 = processRow (stringReader.ReadLine()), givenStrategyMapper

stringReader.Close |> ignore |> stringReader.Dispose

// printf $"%s{givenStrategy1.OpponentMove.ToString()}, %s{givenStrategy1.YourMove.ToString()}{Environment.NewLine}"
// printf $"%s{givenStrategy2.OpponentMove.ToString()}, %s{givenStrategy2.YourMove.ToString()}{Environment.NewLine}"
// printf $"%s{givenStrategy3.OpponentMove.ToString()}, %s{givenStrategy3.YourMove.ToString()}{Environment.NewLine}"

    


let totalScore = 1

let main =
    printf $"{Environment.NewLine}Day 2 Solution: {Environment.NewLine}"
    printf $"Total Score I would earn would be: %i{totalScore}.{Environment.NewLine}{Environment.NewLine}"
