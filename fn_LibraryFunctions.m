(libraryAbsolutePath as text) as record =>
let
    LibraryPath = try libraryAbsolutePath otherwise "C:\temp"
,
    Files = Folder.Files(LibraryPath ),
    #"Filtered PQ" = Table.SelectRows(Files, each ([Extension] = ".pq")),
    #"+FunctionName" = Table.AddColumn(#"Filtered PQ", "FunctionName", each Text.Start([Name], Text.Length([Name])-3)),
    #"+Code" = Table.SelectColumns(Table.AddColumn(#"+FunctionName", "Code", (rec)=> Text.FromBinary(rec[Content])), {"FunctionName", "Code"}),
    #"Core Functions" = List.Accumulate( Table.ToRecords(#"+Code"), [], (state,current)=>
try Record.AddField(state, current[FunctionName], Expression.Evaluate(current[Code], #shared))
otherwise state
),
    #"Non-Core Code" = Table.SelectRows(#"+Code", (row)=> List.Contains( Record.FieldNames(#"Core Functions"), row[FunctionName]) = false),
    #"Extended Functions" = try List.Accumulate( Table.ToRecords(#"Non-Core Code"), #"Core Functions", (state, current) =>
try Record.AddField(state, current[FunctionName], Expression.Evaluate(current[Code], Record.Combine({#shared, #"Core Functions"})))
otherwise Record.AddField(state, current[FunctionName], "Could not evaluate code without error: ")
),
    OutputRecord_Functions = if #"Extended Functions"[HasError] then #"Extended Functions"[Error] else #"Extended Functions"[Value],
    CountFunctions = try Table.RowCount(#"Filtered PQ") otherwise 0,
    OutputRecord_Base = [LibraryPath = """" & LibraryPath & """" & (if #"CountFunctions" = 0 then " has no *.pq files !" else "")],
    Output = Record.Combine({ #"OutputRecord_Base", OutputRecord_Functions})
in
    Output