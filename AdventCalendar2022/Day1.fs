namespace AdventCalendar2022

module public Day1 =

    open System

    [<Measure>] type private Calorie

    type private Food = {
        IndividualCalories: int<Calorie> list
        TotalCalories: int<Calorie>
    }

    type private Elf = {
        Id: int
        Food: Food
    }

    let rec private elfCalorieCounter list: int<Calorie> =
       match list with
       | head :: tail -> head + elfCalorieCounter tail
       | [] -> 0<Calorie>


    let private elfCreator id calorieList =
        let elfTotalCalories = elfCalorieCounter calorieList
        {Id= id; Food={IndividualCalories=calorieList; TotalCalories=elfTotalCalories}}

    let private elf1IndividualCalories = [1000<Calorie>; 2000<Calorie>; 3000<Calorie>]
    let private elf1 = elfCreator 1 elf1IndividualCalories

    let private elf2IndividualCalories = [4000<Calorie>]
    let private elf2 = elfCreator 2 elf2IndividualCalories

    let private elf3IndividualCalories = [5000<Calorie>; 6000<Calorie>]
    let private elf3 = elfCreator 3 elf3IndividualCalories

    let private elf4IndividualCalories = [7000<Calorie>; 8000<Calorie>; 9000<Calorie>]
    let private elf4 = elfCreator 4 elf4IndividualCalories


    let private elf5IndividualCalories = [10000<Calorie>]
    let private elf5 = elfCreator 5 elf5IndividualCalories

    let private elves = [elf1; elf2; elf3; elf4; elf5]

    let private maxElf = query {
        for elf in elves do
        sortByDescending elf.Food.TotalCalories
        select elf
        head
    }

    let public main =
        printf $"{Environment.NewLine}Day 1 Solution: {Environment.NewLine}"
        printf $"Which elf would we ask?  We would ask Elf #%i{maxElf.Id}.{Environment.NewLine}{Environment.NewLine}"