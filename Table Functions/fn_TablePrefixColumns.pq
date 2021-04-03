let func = 
(table as table, prefices as list)=>
//
// TablePrefixColumns
//
// Renames columns with prefices.
// --------------------------------
// The prefices list items are records with [table = comparison table, prefix = "prefix"]
// If a column in the TABLE is contained in one of the PREFICES[TABLE], then the PREFIX is set
// The first prefix is retained

let
	ren  = List.Accumulate(prefices, table, (tbl, cmp)=> 
							Table.TransformColumnNames( tbl, 
									(col)=> if List.Contains(Table.ColumnNames(cmp[table]), col) then 
												cmp[prefix] & "." & col
											else
												col
							)
			),
	result = ren
					
in
    result
    ,
    documentation = [
    Documentation.Name =  " fn_TablePrefixColumns ",
    Documentation.Description = " Renames columns with prefices ",
    Documentation.LongDescription = " The prefices list items are records with [table = comparison table, prefix = ""prefix""] #(lf) If a column in the TABLE is contained in one of the PREFICES[TABLE], then the PREFIX is set
#(lf) The first prefix is retained ",
    Documentation.Category = " Other ",
    Documentation.Source = " www.lehela.com ",
    Documentation.Version = " 1.0 ",
    Documentation.Author = " Leif Lange ",
    Documentation.Examples = {
       [    Description  = "  ",
            Code   = "  ",
            Result = "  "]
      }]
  
 in  
    Value.ReplaceType(func, Value.ReplaceMetadata(Value.Type(func), documentation))	