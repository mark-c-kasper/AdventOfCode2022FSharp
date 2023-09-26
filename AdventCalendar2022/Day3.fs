module AdventCalendar2022.Day3

    open System
    
    let day3input = """vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"""

    let inputList = day3input.Split Environment.NewLine
    
    let alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    
    // let printInputListLength (inputList: string array) =
    //     for input in inputList do
    //         printfn $"{input} length is: {input.Length}"
            
    // type SplitInput = string * string
    
    // let inputSplitter (inputList: string array) =
    //     [
    //     for input in inputList do
    //         let length = input.Length / 2  // Since everything here is even.
    //         let firstHalf = input.Substring(0, length)
    //         let secondHalf = input.Substring(length)
    //         SplitInput ( firstHalf, secondHalf )
    //     ]
        
    let inputSplitterSingle (input: string) =
        let length = input.Length / 2  // Since everything here is even.
        let firstHalf = input.Substring(0, length)
        let secondHalf = input.Substring(length)
        ( firstHalf, secondHalf )
        
    // let printSplitInput (splitInputList: SplitInput list) =
    //     for splitInput in splitInputList do
    //         printfn $"{splitInput}"
            
    // let findSplitOverlappingCharacters (input1: string) (input2: string) =
    //     [
    //         for c in input1.ToCharArray() do
    //             if input2.Contains c then c
    //     ]
        
    let findSplitOverlappingCharacters (input: string * string) =
        let input1,input2 = input
        let characterList =
            [
                for c in input1.ToCharArray() do
                    if input2.Contains c then c
            ]
        Set<char>(characterList)
        
        
    // let printOverlappingCharacters (overlappingCharacters: Set<char> list) =
    //     for list in overlappingCharacters do
    //         printfn $"%A{list}"
            
    let calculateOverlappingTotal (overlappingCharacters: Set<char> array) =
        let mutable total = 0
        for set in overlappingCharacters do
            for i in set do
                total <- total + alpha.IndexOf(i) + 1
        total
            
    // let findSplitOverlappingCharactersForList (splitInputList: SplitInput list) =
    //     [
    //         for splitInput in splitInputList do
    //             let (input1, input2) = splitInput
    //             let characterList = findSplitOverlappingCharacters input1 input2
    //             Set<char>(characterList)
    //     ]

    let public main =
        printfn $"{Environment.NewLine}Day 3 Solution:"
        
        let calculatedTotal =
            inputList
            |> Array.map inputSplitterSingle
            |> Array.map findSplitOverlappingCharacters
            |> calculateOverlappingTotal
        printfn $"Sum of priority items in both compartments is: %i{calculatedTotal}"
