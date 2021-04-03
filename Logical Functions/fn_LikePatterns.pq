(candidate as record, patterns as table, matchcolumns as list, resultcolumns as list, optional wildcard as text) as any =>
let
// Setup debug values
    Candidate = try candidate otherwise 
        [ColA = "This is a heavy test", ColB = ".. and my description"],
    Patterns = try patterns otherwise Table.FromRecords(
        {
        [Name = "%", Description = "%", Weight = 1],
        [Name = "%heavy%", Description = "%", Weight = 5],
        [Name = "%heavy%", Description = "%tor%", Weight = 10]
        }),
    MatchColumns = try matchcolumns otherwise {{"Name","ColA"}, {"Description", "ColB"}},
    ResultColumnNames = try resultcolumns otherwise {"Weight"},
    WildCard = try if wildcard = null or wildcard = "" then "%" else wildcard otherwise "%",

// Test the Candidate against each of the patterns
    PatternsIndexed = Table.AddIndexColumn(Patterns, "Index", 1),
    TestResults = Table.AddColumn(PatternsIndexed, "Test", (Pattern)=> 
List.Accumulate(MatchColumns, true, (state, pair)=>
state and fn_Like(Record.Field(Candidate, pair{1}), Record.Field(Pattern, pair{0}), WildCard)
)
),
// Get the last successful pattern
    MaxIndex = List.Max(Table.SelectRows(TestResults, (rec)=> rec[Test] = true)[Index]),
    Result = Record.SelectFields(Table.SelectRows(TestResults, (rec)=> rec[Index] = MaxIndex){0}, ResultColumnNames)
in
    Result