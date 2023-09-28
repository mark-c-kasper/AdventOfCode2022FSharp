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
        
    let rec private calculateMarkerPos (row:string) uniqueMarkerPosition =
        let isStringUnique = isStringUniqueSetOfCharacters (row.Substring(0, 4))
        match isStringUnique with
        | true -> uniqueMarkerPosition
        | false -> calculateMarkerPos (row.Substring(1)) uniqueMarkerPosition+1
    
    let private processRow (row: string) =
        let position = calculateMarkerPos row 4
        printfn $"Row: %s{row}: first marker after character %i{position}"
        ()

    let public main =
        printfn $"{Environment.NewLine}Day 6 Solution:"
        day6Input.Split Environment.NewLine
        |> Array.map processRow
        |> ignore
    