(tblName as text, 
   optional removePrefix as logical,
   optional lowerCase as logical, 
   optional space_replacer_token as text, 
   optional context as record
) as table =>
let
    TableName = try tblName otherwise "RawData",
    RemovePrefixColumns = try if removePrefix = null or removePrefix = "" then false else removePrefix otherwise false,
    SpaceToken = try if space_replacer_token = null or space_replacer_token = "" then " " else space_replacer_token otherwise " ",
    LowerCase = try if lowerCase = null or lowerCase = "" then false else lowerCase otherwise false,
    Context = try if context = null then [] else context otherwise [],
    FullContext = Record.Combine({#shared, Context}),
    Content = Expression.Evaluate("#""" & TableName & """", FullContext),
    AddPrefix = List.Transform(Table.ColumnNames(Content), (col)=> {col, Text.Replace(TableName & "." & col, " ", SpaceToken)}),
    RemovePrefix = List.Transform(Table.ColumnNames(Content), (col)=> {col, Text.Replace(col, TableName & ".", "")}),
    OutputTable = Table.RenameColumns(Content, if RemovePrefixColumns = false then AddPrefix else RemovePrefix),
    LowerCaseTable = Table.RenameColumns(OutputTable, List.Transform(Table.ColumnNames(OutputTable), (col)=> {col, Text.Lower(col)})),
    Output = if LowerCase = true then LowerCaseTable else OutputTable
in
    Output