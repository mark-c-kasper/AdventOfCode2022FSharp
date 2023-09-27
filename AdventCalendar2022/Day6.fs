module AdventCalendar2022.Day6

    open System

    let private day6Input = """mjqjpqmgbljsphdztnvjfqwrcgsmlb
bvwbjplbgvbhsrlpgdmjqwftvncz
nppdvjthqldpwncqszvftbrmjlhg
nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg
zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"""

    let private isStringUniqueSetOfCharacters (unverifiedString: string) =
        let res = query {
            for c in unverifiedString.ToCharArray() do
                select c
                distinct
        }
        let set = Set res
        set.Count = 4
    
    let private calculateMarkerPosition (row: string) =
        let mutable i = 0
        let mutable markerPositionFound = false
        while (i <= row.Length-4) && not markerPositionFound do
            let unverifiedString = row.Substring(i, 4)
            if isStringUniqueSetOfCharacters unverifiedString then
                markerPositionFound <- true
            else i <- i+1
        i + 4
    
    let private processRow (row: string) =
        let position = calculateMarkerPosition row
        printfn $"Row: %s{row}: first marker after character %i{position}"
        ()

    let public main =
        printfn $"{Environment.NewLine}Day 6 Solution:"
        day6Input.Split Environment.NewLine
        |> Array.map processRow
        |> ignore
        
        
    