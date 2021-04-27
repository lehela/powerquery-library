let 

func = 
    (value as any, optional compressionType as number, optional encoding as nullable number) as text =>
    let
        fn_Main = 
            (value as any, compressionType as number, encoding as nullable number) as text =>
            let
                In = try value otherwise [Value = #time(10,5,1), Assert = "Pass"],
                Compr = try if compressionType is null then -1 else compressionType otherwise -1,
                Enc = try if encoding is null then TextEncoding.Unicode else encoding otherwise TextEncoding.Unicode,
                Serialized = fn_SerializeRecursively(In),
                Expr = Text.Combine({"= " & Serialized}),
                Compress = fn_Compress(Expr, Compr, Enc),
                Test = if Compr <> -1 then 
            Expression.Evaluate(Text.Range(Compress, 2, Text.Length(Compress)-2), #shared)
            else
            Expression.Evaluate(Compress, #shared),
                Out = Compress
            in
                Out
    ,
        lst_Functions = 
            let
                Functions = {
            [Type = Table.Type, FromText = [Function = (v)=>v, Arguments = {"$VAL"}], ToText = [Function = fn_TableToText, Arguments = {"$VAL"}]], 
            [Type = Record.Type, FromText = [Function = (v)=>v, Arguments = {"$VAL"}], ToText = [Function = fn_RecordToText, Arguments = {"$VAL"}]], 
            [Type = List.Type, FromText = [Function = (v)=>v, Arguments = {"$VAL"}], ToText = [Function = fn_ListToText, Arguments = {"$VAL"}]], 
            [Type = Binary.Type, FromText = [Function = Binary.FromText, Arguments = {"$VAL"}], ToText = [Function = Binary.ToText, Arguments = {"$VAL"}]], 
            [Type = Date.Type, FromText = [Function = Date.FromText, Arguments = {"$VAL"}], ToText = [Function = Date.ToText, Arguments = {"$VAL"}]], 
            [Type = Number.Type, FromText = [Function = Number.FromText, Arguments = {"$VAL"}], ToText = [Function = Number.ToText, Arguments = {"$VAL"}]], 
            [Type = DateTime.Type, FromText = [Function = DateTime.FromText, Arguments = {"$VAL"}], ToText = [Function = DateTime.ToText, Arguments = {"$VAL"}]],
            [Type = Time.Type, FromText = [Function = Time.FromText, Arguments = {"$VAL"}], ToText = [Function = Time.ToText , Arguments = {"$VAL", "hh:mm:ss"}]],
            [Type = Logical.Type, FromText = [Function = Logical.FromText, Arguments = {"$VAL"}], ToText = [Function = Logical.ToText, Arguments = {"$VAL"}]], 
            [Type = Duration.Type, FromText = [Function = Duration.FromText, Arguments = {"$VAL"}], ToText = [Function = Duration.ToText, Arguments = {"$VAL"}]], 
            [Type = Text.Type, FromText = [Function = (val)=>val, Arguments = {"$VAL"}], ToText = [Function = fn_PrepText ] , Arguments = {"$VAL"}]
            }
            in
                Functions
    ,
        fn_FunctionName = (f as function) => try Value.ResourceExpression(f)[Name] otherwise null
    ,
        rec_EvalEnvironment =
            let
                Functions = lst_Functions,
                Environments = List.Accumulate(Functions, [], (state, current) => 
                    try Record.AddField(state, fn_FunctionName(current[FromText][Function]), current[FromText][Function] )
                    otherwise state
                ),
                Table.FromRecords = Record.AddField(Environments, "Table.FromRecords", Table.FromRecords)
            in
            Table.FromRecords
    ,
        fn_Args = 
            (args as list, val as any) as list => List.Transform(args, (itm)=> try if itm = "$VAL" then val else itm otherwise itm )
    ,
        fn_ToText =
            (fromText as record, toText as record, val as any ) as text =>
            let
                fromText = try fromText otherwise lst_Functions{6}[FromText],
                toText = try toText otherwise lst_Functions{6}[ToText],
                val = try val otherwise [Value = #time(10,5,1), Assert = "Pass"],
                pattern = Text.Replace(
                    Text.Replace("$FT($ARGS)", "$ARGS", Text.Combine(fromText[Arguments],","))
                    , "$VAL", """$VAL"""),
                args = fn_Args( toText[Arguments] ,val),
                replaceFromText = Text.Replace(pattern, "$FT", fn_FunctionName(fromText[Function])),
                replaceVal = Text.Replace(replaceFromText, "$VAL", Function.Invoke(toText[Function], args )),
                out = try replaceVal otherwise Function.Invoke(toText[Function], {val})
            in
                out
    ,
        fn_PrepText = 
            (val as text) as text =>
            let
                In1 = val, 
                Test = Value.Type(Int16.From("4")),
                TestText = "Normal Test",
                TestQuote = """""Text with Quotes""",
                Input = try In1 otherwise TestText,
                SpecialChars = {""""},
                Convert = List.Accumulate(SpecialChars, Input, (state, current)=>
            Text.Replace(state, current, Text.Combine({current,current}))
            ),
                Output = Text.Combine({"""", Convert,""""})
            in
                Output
    ,
        fn_ListToText = 
            (lst as list) as text =>
            let
                In1 = try lst otherwise {"Normal Text", """Hello"""}
            ,
                Source = In1,
                FieldsToText = List.Transform(Source, (val)=> fn_SerializeRecursively(val)),
                ListToText = Text.Combine({ "{", Text.Combine(FieldsToText, ","), "}"}),
                Output = ListToText
            in
                Output
    ,
        fn_RecordToText = 
            (rec as record) as text =>
            let
                In1 = try rec otherwise [Col1 = "Normal Text", Col2 = null, Col3 = """Hello"""]
            ,
                Source = In1,
                Table = Record.ToTable(Source),
                FieldsToText = Table.TransformRows(Table, (row)=>
                "#""" & row[Name] & """ = " & fn_SerializeRecursively(row[Value])
            ),
                RecordToText = Text.Combine({ "[", Text.Combine(FieldsToText, ","), "]"}),
                Output = RecordToText
            in
                Output
    ,
        fn_TableToText = 
            (tbl as table) as text =>
            let
                In1 = try tbl otherwise Table.FromRows({{"A","B"}},{"ColA", "ColB"}),
                Source = In1,
                #"List Records" = List.Transform(Table.ToRecords(  Source), fn_RecordToText),
                TableToText = Text.Combine({"Table.FromRecords({", Text.Combine(#"List Records", ","), "})"}),
                Output = TableToText
            in
                Output
    ,
        fn_Compress = 
            (value as text, optional compressionType as number, optional encoding as nullable number) as text =>
            let
                In = try value otherwise "= [Test = 1]",
                InExpr = if Text.Start(In, 2) = "= " then Text.Range(In, 2, Text.Length(In)-2) else In,
                Compr = try if compressionType is null then -1 else compressionType otherwise Compression.GZip,
                Enc = try if encoding is null then TextEncoding.Unicode else encoding otherwise TextEncoding.Unicode,
                DecompressTemplate = "= Expression.Evaluate(Text.FromBinary(Binary.Decompress(Binary.From(""$VAL""), $COMPRESSION), $TEXTENCODING), #shared)",
                CompressedBin = Binary.ToText(Binary.Compress(Text.ToBinary(InExpr, Enc), Compr)),

                CompressedM = Text.Replace(
            Text.Replace(
            Text.Replace(DecompressTemplate
            , "$COMPRESSION", Text.From(Compr))
            , "$TEXTENCODING", Text.From(Enc))
            , "$VAL", Text.From(CompressedBin)),
                TestCompressedM = Expression.Evaluate(Text.Replace(CompressedM, "= ",""), #shared),
                Out = if Compr <> -1 then CompressedM else InExpr
            in
                Out
    ,
        fn_SerializeRecursively = 
            (value as any) as text =>
            let
                In = try value otherwise [Value = #time(10,5,1), Assert = "Pass"],
                Serialize = List.Accumulate(lst_Functions, [Value = In , Done = false] , (state, current)=>
                    if state[Done] then state  
                    else if state[Value] = null then [ Value = "null", Done = true]
                    else if Value.Is(state[Value], current[Type]) then [ Value = fn_ToText(current[FromText], current[ToText], state[Value]) , Done = true]
                    else state
                    )[Value],
                Check = try Text.From(Serialize),
                Output = if Check[HasError] then Text.Combine({"""",  Check[Error][Message], """"}) else Serialize
            in
                Output
    ,   
        Compr = try if compressionType is null then -1 else compressionType otherwise -1,
        Enc = try if encoding is null then TextEncoding.Unicode else encoding otherwise TextEncoding.Unicode,
        Output = fn_Main(value, Compr, Enc)
    in
        Output
,
documentation = 
    [
        Documentation.Name =  "<b>fn_Serialize</b>",
        Documentation.Description = " Serializes any value into an M Script",
        Documentation.LongDescription = " 
        <b>Serializes any value into an M Script that can be evaluated.</b>
        </br></br>
        To reduce the script size for longer statements, specify a compression:
        <ul>
            <li><code>Compression.GZip</code></li>
            <li><code>Compression.Deflate</code></li>
        </ul>
        The standard encoding for compressed scripts is <code>TextEncoding.Unicode</code>, but a separate encoder may be specified.",
        Documentation.Category = " Debug, Testing ",
        Documentation.Source = " www.lehela.com ",
        Documentation.Version = " 1.1 ",
        Documentation.Author = " Leif Lange ",
        Documentation.Examples = {
        [    Description  = "  ",
                Code   = "  ",
                Result = "  "]}
    ]
 in  
    Value.ReplaceType(func, Value.ReplaceMetadata(Value.Type(func), documentation))