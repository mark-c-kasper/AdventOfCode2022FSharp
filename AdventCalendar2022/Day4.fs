module AdventCalendar2022.Day4
    
    open System
    
    let private day4Input = """2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"""

    type private Segment = {
        min: int
        max: int
    }

    type private PairedSegment = {
        FirstSegment: Segment
        SecondSegment: Segment
    }
    
    let private commaSegmentSplitter (unSplitSegment: string) =
        let segments = unSplitSegment.Split ','
        let pairedSegment =
            {
                // Never a fan of hard-coding anything; however, I will make exceptions for this type of situation.
                FirstSegment={ min= int (segments[0].Substring(0,1)); max=int (segments[0].Substring(2))};
                SecondSegment={ min= int (segments[1].Substring(0,1)); max=int (segments[1].Substring(2))}
            }
        pairedSegment
        
    let private isOnePairedAssignmentFullyContainedInAnother pairedSegment =
        if (pairedSegment.FirstSegment.min <= pairedSegment.SecondSegment.min
            && pairedSegment.FirstSegment.max >= pairedSegment.SecondSegment.max)
            || (pairedSegment.SecondSegment.min <= pairedSegment.FirstSegment.min
                && pairedSegment.SecondSegment.max >= pairedSegment.FirstSegment.max)
        then 1
        else 0
        
    let public main =
        let totalOverlappingPairs =
            day4Input.Split Environment.NewLine
            |> Array.map commaSegmentSplitter
            |> Array.map isOnePairedAssignmentFullyContainedInAnother
            |> Array.sum
        
        printfn $"{Environment.NewLine}Day 4 Solution:"
        printfn $"Number of overlapping pairs is: %i{totalOverlappingPairs}"