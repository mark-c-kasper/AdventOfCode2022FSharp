module AdventCalendar2022.Day9

    open System
    
    let private day9Input = """R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2"""

    type private Direction =
        | U
        | L
        | R
        | D

    type private Head = int * int
    type private Tail = int * int
    type private Move = Direction * int
    
    let mutable private tailHistory: Set<Tail> = Set.empty
    
    let mutable private head: Head = 0, 0
    let mutable private tail: Tail = 0, 0
        
    let mutable private bridge: char array array = Array.init 6 (fun i -> (Array.init 5 (fun p -> ' ')))
    
    let private printBridge bridge =
        for row in bridge do
            printfn $"%A{row}"
    
    let private getMoveDirection moveDirection =
        match moveDirection with
        | "U" -> Direction.U
        | "L" -> Direction.L
        | "R" -> Direction.R
        | "D" -> Direction.D
        | _ -> raise (Exception "Bad match!")
    
    let private parseMove (move: string) : Move =
        let direction = getMoveDirection (move.Substring(0, 1))
        let moveCount = int (move.Substring(2, 1))
        (direction, moveCount)
        
    let private getTailsMoveDirection headVal tailVal =
        if headVal > tailVal then 1 else -1
        
    let private moveTailsIfNeeded (head: Head) =
        let headRow, headColumn = head
        let tailRow, tailColumn = tail
        if abs(headRow - tailRow) > 1
        then
            let tailYPos = if headColumn <> tailColumn then headColumn else tailColumn
            let moveCnt = getTailsMoveDirection headRow tailRow
            let returnTail: Tail = (tailRow + moveCnt, tailYPos)
            tailHistory <- tailHistory.Add(returnTail)
            tail <- returnTail
        else if abs(headColumn - tailColumn) > 1
        then
            let tailXPos = if headRow <> tailRow then headRow else tailRow
            let moveCnt = getTailsMoveDirection headColumn tailColumn
            let returnTail: Tail = (tailXPos, tailColumn + moveCnt)
            tailHistory <- tailHistory.Add(returnTail)
            tail <- returnTail
        else
            tailHistory <- tailHistory.Add(tail)
    
    let private setHeadAndTailPositionToBlank (bridge: char array array) =
        let headRow, headColumn = head
        let tailRow, tailColumn = tail
        bridge[headRow][headColumn] <- ' '
        // printfn $"%c{bridge[headRow][headColumn]}"
        if headRow <> tailRow || headColumn <> tailColumn
        then bridge[tailRow][tailColumn] <- ' '
    
    let private horizontalIncrement direction =
        let headRow, headColumn = head
        head <- headRow + direction, headColumn
    
    let private verticalIncrement direction =
        let headRow, headColumn = head
        head <- headRow, headColumn + direction
        
    let private moveHead (moveCount: int) (bridge: char array array) direction incrementFunc =
        for i = 1 to moveCount do
            setHeadAndTailPositionToBlank bridge
            incrementFunc direction
            moveTailsIfNeeded head
        
    let private setHeadAndTailPosition (bridge: char array array) =
        let headRow, headColumn = head
        let tailRow, tailColumn = tail
        bridge[headRow][headColumn] <- 'H'
        // printfn $"%c{bridge[headRow][headColumn]}"
        if headRow <> tailRow || headColumn <> tailColumn
        then bridge[tailRow][tailColumn] <- 'T'
    
    let private processMove (move: Move) (bridge: char array array) =
        let direction, moveCount = move
        match direction with
        | U -> moveHead moveCount bridge 1 verticalIncrement
        | D -> moveHead moveCount bridge -1 verticalIncrement
        | L -> moveHead moveCount bridge -1 horizontalIncrement
        | R -> moveHead moveCount bridge 1 horizontalIncrement
    
    let public main =
        printfn $"{Environment.NewLine}Day 9 Solution:"
        
        setHeadAndTailPosition bridge
        // printBridge bridge
        
        let moves = day9Input.Split Environment.NewLine
        
        for move in moves do
            processMove (parseMove move) bridge
            setHeadAndTailPosition bridge
            // printBridge bridge
        
        let spacesVisited = tailHistory.Count
        
        printfn $"Total places visited: %i{spacesVisited}"