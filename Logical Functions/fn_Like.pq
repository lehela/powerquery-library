//######fn_Like## This is delimiter. Dont remove it 
let
    func = (phrase as text, pattern as text, optional delim as text) as logical => 
    let
        Debug = [Phrase = "This is my test phrase", Pattern = "%test%", Delimiter = "%", ExpectedResult = true],
        DelimiterDefault = "%",
        Phrase = try phrase otherwise Debug[Phrase],
        Pattern = try pattern otherwise Debug[Pattern],
        Delimiter = try if delim = "" then DelimiterDefault else delim otherwise Debug[Delimiter],
        #"Split Pattern Pattern" = Text.Split(Pattern, Delimiter),
        #"Parse Phrase" = Table.FromRecords(List.Accumulate(
            #"Split Pattern Pattern", 
            {[Pattern = Pattern, Token = null, BeforeToken = null, AfterToken = Phrase]}, 
            (state, token)=> 
                List.Combine({state, {
                let
                    PreviousPhrase = List.Last(state)[AfterToken],
                    SplitPhrase = Text.Split(PreviousPhrase , token){0}, 
                    NextPhrase = if Text.Length(token) = 0 
                                    then PreviousPhrase 
                                    else Text.RemoveRange(PreviousPhrase, 0, Text.Length(SplitPhrase & token) ),
                    Output = [Pattern = Pattern, Token = token, BeforeToken = SplitPhrase, AfterToken = NextPhrase]
                in 
                    Output
                }})
            )),
        #"Removed First Row" = Table.Skip(#"Parse Phrase",1),
        #"Added Index" = Table.AddIndexColumn(#"Removed First Row", "Index", 1, 1),
        #"Check Start Matches" = (Table.First(#"Added Index")[Token]<>"" and Table.First(#"Added Index")[BeforeToken] = "")
    or Table.First(#"Added Index")[Token] = "",
        #"Check End Matches" = (Table.Last(#"Added Index")[Token]<>"" and Table.Last(#"Added Index")[AfterToken] = "")
    or Table.Last(#"Added Index")[Token] = "",
        #"Check No Errors" = Table.RowCount(Table.SelectRowsWithErrors(#"Removed First Row"))=0,
        // End Result
        Result = #"Check Start Matches" and
    #"Check End Matches" and
    #"Check No Errors"
    in
        Result,
    documentation = [
        Documentation.Name = " fn_Pattern ",
        Documentation.Description = " Pattern operator similar to SQL",
        Documentation.LongDescription = " Returns boolean true if the phrase matches the pattern. The default delimiter is '%'. " & #"Unit Tests",
        Documentation.Category = " Other ",
        Documentation.Source = " https://github.com/lehela/powerquery-library ",
        Documentation.Version = " 1.0 ",
        Documentation.Author = " Leif Lange ",
        Documentation.Examples = {
            [
                Description = " Match ",
                Code = " fn_Pattern(""Example sentence"", ""%ente%"")  ",
                Result = " true "
            ],
            [
                Description = " Match with custom delimiter",
                Code = " fn_Pattern(""Example sentence"", ""Exam*"", ""*"")  ",
                Result = " true "
            ],            
            [
                Description = "  No Match ",
                Code = " fn_Pattern(""Example sentence"", ""%ante%"")  ",
                Result = " true "
            ]
        }
    ],
    #"Unit Tests" = 
        let
            Phrase = Table.FromRecords({
        [Phrase = "This is my test phrase", Pattern = "%test%", Delimiter = "%", ExpectedResult = true],
        [Phrase = "This is my test phrase", Pattern = "test%", Delimiter = "%", ExpectedResult = false],
        [Phrase = "This is my test phrase", Pattern = "%test%ase", Delimiter = "%", ExpectedResult = true],
        [Phrase = "This is my test phrase", Pattern = "%test%as", Delimiter = "%", ExpectedResult = false],
        [Phrase = "This is my test phrase", Pattern = "This%is% test%", Delimiter = "%", ExpectedResult = true],
        [Phrase = "This is my test phrase", Pattern = "This is my test phrase", Delimiter = "%", ExpectedResult = true],
        [Phrase = "This is my test phrase", Pattern = "*test*", Delimiter = "*", ExpectedResult = true],
        [Phrase = "This is my test phrase", Pattern = "test*", Delimiter = "*", ExpectedResult = false],
        [Phrase = "This is my test phrase", Pattern = "*test*ase", Delimiter = "*", ExpectedResult = true],
        [Phrase = "This is my test phrase", Pattern = "*test*as", Delimiter = "*", ExpectedResult = false],
        [Phrase = "This is my test phrase", Pattern = "This is my test phrase", Delimiter = "*", ExpectedResult = true]
        }),
            // My Unit Tests
            #"Unit Tests" = Table.AddColumn(Phrase, "Unit Test", (test)=> if func(test[Phrase], test[Pattern], test[Delimiter]) = test[ExpectedResult] then "Pass" else "Fail"),
            #"Filter Failed Tests" = Table.SelectRows(#"Unit Tests", each [Unit Test] = "Fail"),
            Output = if Table.RowCount(#"Filter Failed Tests") = 0 
        then "All Unit Tests are passing."
        else "Error! Some Unit Tests are failing!"
        in
            Output   
in
    Value.ReplaceType(
        func,
        Value.ReplaceMetadata(
            Value.Type(func),
            documentation
        )
    )