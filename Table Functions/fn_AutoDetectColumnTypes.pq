//
// AutoDetectColumnTypes
//
(table as table, optional samples) =>
let
	data = table,
	smpsize = if samples = null then 20 else samples,
	transcol = (tbl as table, col as text)=>
		let
			types = {type datetime, type date, type logical, type number, type text},
			eval = List.Transform(types, (tp)=> [typ=tp, res=List.Accumulate(List.FirstN(Table.Column(tbl, col), smpsize), null, (status, val)=>if val = null then status else if status = null then Value.Is(val, tp) else status and Value.Is(val, tp))]),
			auto = try List.First( List.Select(eval, (rec)=> rec[res]=true))[typ] otherwise null,
			typ = if auto <> null then auto else type any
		in
			typ,
	exe = Table.TransformColumnTypes(data, List.Transform(Table.ColumnNames(data), (col)=> {col, transcol(data, col)}))
in
	exe