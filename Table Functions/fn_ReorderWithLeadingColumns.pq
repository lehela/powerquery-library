// Dynamically reorders the tbl columns by placing the list of leadingColumns first
// All remaining columns are kept in their original order after the leading columns
(tbl as table, leadingColumns as list)=>
let
    Source = try tbl otherwise #table({"First", "Second", "Third"}, {{1,2,3},{"A","B","C"}}),
    LeadCols = try leadingColumns otherwise List.RemoveFirstN(Table.ColumnNames(Source)),
    Input = [Source = Source, LeadCols = LeadCols],
    Reorder = Table.ReorderColumns(Input[Source], List.Combine({Input[LeadCols], List.Select(Table.ColumnNames(Input[Source]), (val)=> not List.Contains(Input[LeadCols], val))}))
in
    Reorder