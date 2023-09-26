module AdventCalendar2022.Day2Redo

open Microsoft.FSharp.Core

type Move =
    | Rock
    | Paper
    | Scissor
    
// I could remove this to simplify the data model to only work with the points, but I feel this adds nicely to readability.
// This is also just a fun side coding project.
type WinLoseDraw =
    | Win
    | Lose
    | Draw

type MoveSet = Move * Move

type PossibleMove = {
    Moves: MoveSet
    PointsForMove: int
    RoundWinLoseDraw: WinLoseDraw
    WinLoseDrawPoint: int
}

type PossibleMoveList = PossibleMove list

let playPoint move =
    match move with
    | Rock -> 1
    | Paper -> 2
    | Scissor -> 3
    
let winLoseDrawPoint wld =
    match wld with
    | Win -> 6
    | Lose -> 0
    | Draw -> 3

let roundResult oMove yMove : WinLoseDraw =
    if oMove.Equals yMove
    then Draw
    else if (yMove.Equals Rock && oMove.Equals Scissor)
            || (yMove.Equals Paper && oMove.Equals Rock)
            || (yMove.Equals Scissor && oMove.Equals Paper)
    then Win
    else Lose

let loadPossibleMoves : PossibleMoveList =
    let allMoves = [Rock; Paper; Scissor]
    [
    for oMove in [Rock; Paper; Scissor] do
        for yMove in allMoves do
            let result = roundResult oMove yMove
            let point = playPoint yMove
            let wldPoint = winLoseDrawPoint result
            {Moves=(oMove, yMove); PointsForMove = point; RoundWinLoseDraw = result ; WinLoseDrawPoint = wldPoint }
    ]

// I don't really feel like doing a read of this and breaking it down further, so I'm just manually coding the
// given strategy and the optimal strategy.
let givenStrategy =
    """A Y
B X
C Z"""

    // if move = 'A' then rockMove else if move = 'Y' then paperMove
    // // Lose
    // else if move = 'B' then paperMove else if move = 'X' then rockMove
    // // Draw
    // else if move = 'C' then scissorMove else if move = 'Z' then scissorMove
let givenRound1:MoveSet = (Rock, Paper)
let givenRound2:MoveSet = (Paper, Rock)
let givenRound3:MoveSet = (Scissor, Scissor)

// Just saying that we are winning each round.  We really aren't worried about suspicions because victory shall be ours!
let optimalRound1:MoveSet = (Rock, Paper)
let optimalRound2:MoveSet = (Paper, Scissor)
let optimalRound3:MoveSet = (Scissor, Rock)


let rec calculateGameTotal (roundResults: PossibleMoveList) =
    match roundResults with
    | head :: tail ->  head.PointsForMove + head.WinLoseDrawPoint + (calculateGameTotal tail)
    | [] -> 0


let roundResultCalculator rounds (possibleMoves: PossibleMoveList) =
        [
            for round in rounds do
                query {
                for pm in possibleMoves do
                    where (pm.Moves.Equals round)
                    head
                }
        ]

let main =
    let possibleMoves = loadPossibleMoves
    let gameTotal = calculateGameTotal (roundResultCalculator [givenRound1; givenRound2; givenRound3] possibleMoves)
    let optimalGameTotal = calculateGameTotal (roundResultCalculator [optimalRound1; optimalRound2; optimalRound3] possibleMoves)
    
    printfn "Day 2 Solution:"
    printfn $"Given Strategy Point Total: %i{gameTotal}"
    printfn $"Optimal Strategy Point Total: %i{optimalGameTotal}"
    