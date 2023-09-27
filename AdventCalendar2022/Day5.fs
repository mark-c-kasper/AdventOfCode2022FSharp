module AdventCalendar2022.Day5

    open System
    open System.Collections.Generic
    open Microsoft.FSharp.Collections
   

    type private column = Stack<char>
    
    type private Move = {
        numbToMove: int
        sourceColumn: int
        destColumn: int
    }
    
    let private day5Input = """move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"""

    //     [D]    
    // [N] [C]    
    // [Z] [M] [P]
    //  1   2   3
    
    let private givenColumn1: column = Stack<char>(['Z'; 'N'])
    let private givenColumn2 = Stack<char>(['M'; 'C'; 'D'])
    let private givenColumn3 = Stack<char>(['P'])
    
    let private columns = [givenColumn1; givenColumn2; givenColumn3]
    
    // Implemented the simplest approach to solving this problem by just removing the excess and using the single values.
    let private rowParser (row: string) : Move =
        let basicRow = row.Replace("move ", "").Replace(" from ", "").Replace(" to ", "")
        let numbs = int (basicRow.Substring(0, 1))
        let source = int (basicRow.Substring(1, 1))
        let dest = int (basicRow.Substring(2, 1))
        {numbToMove = numbs; sourceColumn = source; destColumn = dest }
    
    let private processInputRow row =
        let move = rowParser row
        for _ in [1..move.numbToMove] do
            let valueToMove = columns[move.sourceColumn-1].Pop()
            columns[move.destColumn-1].Push(valueToMove)
        
    let private printResult (column: column) =
        printf $"%s{column.Peek().ToString()}"
        ()
        
    let private printResults (columns: column list) =
        printf "Results: "
        for column in columns do
            printf $"%s{column.Peek().ToString()}"
        printf $"{Environment.NewLine}"
        ()
    
    let public main =
        printfn $"{Environment.NewLine}Day 5 Solution:"
        day5Input.Split Environment.NewLine
        |> Array.map processInputRow
        |> ignore
        printResults columns
        
    