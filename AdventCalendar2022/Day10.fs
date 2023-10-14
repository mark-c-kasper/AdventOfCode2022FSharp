module AdventCalendar2022.Day10

    open System
    open System.Collections.Generic
    
    type private CycleInput = {
        Cycle: int
        Amount: int
    }
    
    let private day10Input = """addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop"""

    let private printQueue i register (queue: Queue<CycleInput> ) =
        let array = queue.ToArray()
        for a in array do
            printfn $"Iteration: %i{i}, Register: %i{register}, Cycle: %i{a.Cycle}, Amount: %i{a.Amount}"
    
    let public main =
        printfn $"{Environment.NewLine}Day 10 Solution:"
        
        let mutable register = 1
        let mutable sumOfSignalStrengths: int list = []
        let queue = List<CycleInput>()
        
        let commands = day10Input.Split Environment.NewLine
        for i = 1 to 225 do
            if i <= commands.Length then
                let command = commands[i-1]
                
                let commandCycle = if queue.Count > 0 then queue[queue.Count-1].Cycle else 1
                if command.Contains "addx" then
                    let addValue = (command.Split " ")[1]
                    queue.Add {Cycle= commandCycle + 2; Amount= (int addValue)}
                else
                    queue.Add {Cycle = commandCycle + 1; Amount = 0 }
            
            if queue.Count > 0
            then
                if i = queue.[0].Cycle then
                    let head = queue[0]
                    queue.RemoveAt 0
                    printfn $"Adding: %i{head.Amount} to register"
                    register <- register + head.Amount
                
            if i = 20 || i = 60 || i = 100 || i = 140 || i = 180 || i = 220
            then
                sumOfSignalStrengths <- sumOfSignalStrengths @ [(i * register)]
                printfn $"Iteration: %i{i}, Register value: %i{register}, Signal Strength: {i * register}, Queue Count: %i{queue.Count}"
                
        printfn $"Sum of strengths: %i{sumOfSignalStrengths |> List.sum}"
        ()