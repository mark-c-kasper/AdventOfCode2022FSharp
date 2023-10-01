module AdventCalendar2022.Day8

    open System
    
    let private day8Input = """30373
25512
65332
33549
35390"""
    
    type private Grid = int list list
    
    let rec private createColumnList (columns : string) =
        match columns.Length with
        | 0 -> []
        | _ -> [(int (columns.Substring(0, 1)))] @ createColumnList columns[1..]
    
    let rec private createGrid (rows: string array) =
        match rows.Length with
        | 0 -> []
        | _ -> [createColumnList rows[0]] @ createGrid rows[1..]
    
    let private isOnTheEdge row column (grid: Grid) =
        row = 0 || column = 0 || column = grid[row].Length-1 || row = grid[column].Length-1
    
    let rec private isTreeVisibleFromTop i row column (grid: Grid) value =
        if i = row
        then true
        else if i < row && grid[i][column] < value
        then isTreeVisibleFromTop (i+1) row column grid value
        else false
        
    let rec private isTreeVisibleFromBottom i row column (grid: Grid) value =
        if i = grid.Length
        then true
        else if i < grid.Length && grid[i][column] < value
        then isTreeVisibleFromBottom (i+1) row column grid value
        else false
        
    let rec private isTreeVisibleFromLeft i row column (grid: Grid) value =
        if i = column
        then true
        else if i < column && grid[row][i] < value
        then isTreeVisibleFromLeft (i+1) row column grid value
        else false
        
    let rec private isTreeVisibleFromRight i row column (grid: Grid) value =
        if i = grid[row].Length
        then true
        else if i < grid[row].Length && grid[row][i] < value
        then isTreeVisibleFromRight (i+1) row column grid value
        else false
    
    let private countTrees (grid: Grid) =
        [
            for r = 0 to (grid.Length - 1) do
                for c = 0 to (grid[r].Length - 1) do
                    let value = grid[r][c]
                    if isOnTheEdge r c grid then 1
                    else if isTreeVisibleFromTop 0 r c grid value then 1
                    else if isTreeVisibleFromBottom (r+1) r c grid value then 1
                    else if isTreeVisibleFromLeft 0 r c grid value then 1
                    else if isTreeVisibleFromRight (c+1) r c grid value then 1
                    else 0
        ]
    
    let public main =
        printfn $"{Environment.NewLine}Day 8 Solution:"
        let rows = day8Input.Split Environment.NewLine
        let grid = createGrid rows
        // for row in grid do
        //     printfn $"%A{row}"
        let treeCount = countTrees grid |> List.sum
        printfn $"Number of visible trees: %i{treeCount}"
        
        