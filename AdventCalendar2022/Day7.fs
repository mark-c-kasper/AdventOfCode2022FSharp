module AdventCalendar2022.Day7

    open System
    open System.Collections.Generic
    open System.Linq
    
    type Name = string
    
    type Size = int
    
    type File = {
        Name: Name
        Size: Size
    }
       
    type CommandOrOutput =
        | Command
        | Output
    
    let day7Input = """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"""

    let private isCommandOrOutput firstCharacter =
        match firstCharacter with
        | "$" -> Command
        | _ -> Output
        
    let private getOutputSize (output: string) =
        let outputLine = output.Split(" ")
        if outputLine[0] = "dir"
        then 0
        else int outputLine[1]
        
    let private addDirectoryAndChangeReturnPrefix (commandString: string array) (prefix: string) (contents: List<File>) =
        if commandString[2] <> "/" then
            contents.Add({Name = $"%s{prefix}%s{commandString[2]}/"; Size=0 })
            $"%s{prefix}%s{commandString[2]}/"
        else
            contents.Add({Name= "/"; Size=0 })
            "/"
        
    let private buildDirectoryContents (commandsAndOutputs: string array) =
        let mutable fileNamePrefix = String.Empty
        let contents = List<File>()
        for c in commandsAndOutputs do
            match c with
            // Listing contents, we do not care.
            | "$ ls" -> () // NOOP
            // Going up one directory.
            | "$ cd .." -> fileNamePrefix <- fileNamePrefix.Substring(0, fileNamePrefix.LastIndexOf"/"-1)
            | _ ->
                let commandOutputLine = c.Split(" ")
                match commandOutputLine[0] with
                // This said that the parent directory has a directory.  Unless we navigate there, the size is generally 0.
                | "dir" -> () // NOOP fileNamePrefix <- fileNamePrefix //contents.Add($"%s{fileNamePrefix}/%s{commandOutputLine[1]}", 0)
                // Need to show that we're changing the directory with our file names.
                | "$" -> fileNamePrefix <- addDirectoryAndChangeReturnPrefix commandOutputLine fileNamePrefix contents
                // Adding an actual file.
                | _ -> contents.Add( { Name= $"%s{fileNamePrefix}%s{commandOutputLine[1]}"; Size= int (commandOutputLine[0]) })
        contents
    
    let fetchFileSize (files: File * File) =
        let directory, file = files
        file.Size
        
    let filter (files: Name * int) =
        let directory, size = files
        size < 100000
        
    let printDirectorySize (directory: Name * int) =
        let name, size = directory
        printfn $"%s{name}, %i{size}"
        
    let SumOfFilteredDirectories (filteredDirectories: seq<Name * int>) =
        let mutable totalSize = 0
        for fd in filteredDirectories do
            let name, size = fd
            totalSize <- totalSize + size
        totalSize
    
    let private queryEachDirectorySize (directoryContents: List<File>) =
        let filteredDirectories =
            query {
                for d in directoryContents do
                join f in directoryContents
                    on (1 = 1)
                where (f.Name.Contains(d.Name) && (d.Size = 0) && (f.Size > 0))
                groupBy d.Name into directory
                select (directory.Key, directory.Sum(fun d -> fetchFileSize d))
            }
            |> Seq.filter  (fun s -> filter s)
            // |> Seq.iter printDirectorySize
        let sum = SumOfFilteredDirectories filteredDirectories
        printfn $"Total Sum: %i{sum}"
    
    let public main =
        printfn $"{Environment.NewLine}Day 7 Solution:"
        let commandsAndOutputs = day7Input.Split Environment.NewLine
        queryEachDirectorySize (buildDirectoryContents commandsAndOutputs)
        ()