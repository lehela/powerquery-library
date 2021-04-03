// fn_table_transformColumns
//
// Applies column transformations with a row reference
// 
// operations = {{col, function(val, row)}}
// where col is the column to be transformed
// where val is the current value of the column
// where row is the reference to the current record being transformed

(tbl as table, operations as list)=>
let
    tbl = try tbl otherwise Table.FromList({{"A",2,1},{"A",3,4},{"A",2,3}},(lst)=> lst, {"ID", "Orig", "Curr"}),
    operations = try operations otherwise {{"Curr", (val, row)=> val + row[Orig]}, {"Orig", (val, row)=> row[Curr]}},
    Input = tbl,
    Ops = operations,
    #"Apply Operations" = Table.TransformRows(Input, (row)=> 
                                Record.TransformFields(row, 
                                    List.Transform(Ops, (op)=> {op{0}, (val)=> op{1}(val, row)})
                                )
                        )
        ,
    Output = Table.FromRecords(#"Apply Operations")
in
    Output