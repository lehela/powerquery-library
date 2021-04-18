(tbl as table, optional separator as text) as table =>
let
    DebugTable = Table.FromRecords({}),
    DebugAuto = [
            tbl = DebugTable
            , columns = null
            , collapsedName = null
            , separator = "."
            ],
    DebugNormal = [
        tbl = DebugTable
        , columns = {"Content", "Name"}
        , collapsedName = "CollapsedName"
        , separator = "."
        ],
    Debug = DebugAuto,

    Source = try tbl otherwise Debug[tbl],
    Separator = try if separator = null or separator = "" then Debug[separator] else separator otherwise Debug[separator],
    ColumnsToCollapse = try columns otherwise Debug[columns],
    CollapsedName = try collapsedName otherwise Debug[collapsedName],

    #"Collapsable Columns" = List.Select(Table.ColumnNames(Source), (val)=> Text.Contains(val, Separator)),
    #"Split Column Names" = Table.FromRecords(List.Transform(#"Collapsable Columns", (col)=> [Table = Text.Split(col, Separator){0}, Column = col])),
    #"ColumnGroupsAuto" = Table.Group(#"Split Column Names", {"Table"}, {{"Columns", each [Column]}}),

    #"ColumnGroupUser" = {[Table = CollapsedName, Columns = ColumnsToCollapse]},

    #"ProcessGroups" = if (CollapsedName = null or ColumnsToCollapse = null) then #"ColumnGroupsAuto" else #"ColumnGroupUser",
    
    Execute = List.Accumulate(Table.ToRecords(#"ProcessGroups"), Source, (state, current) =>
                Table.RemoveColumns(
                    Table.AddColumn(#"Source", current[Table], 
                    (rec)=> Record.SelectFields(rec, current[Columns])
                    ), 
                    current[Columns])
                ),
    Output = Execute
in
    Execute