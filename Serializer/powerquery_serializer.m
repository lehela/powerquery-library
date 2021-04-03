//######fn_Main## This is delimiter. Dont remove it
(value as any, optional compressionType as number, optional encoding as nullable number) as text =>
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

//######fn_SerializeRecursively## This is delimiter. Dont remove it
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

//######lst_Functions## This is delimiter. Dont remove it
let
    Functions = {
[Type = Table.Type, FromText = [Function = (v)=>v, Arguments = {"$VAL"}], ToText = [Function = fn_TableToText, Arguments = {"$VAL"}]], 
[Type = Record.Type, FromText = [Function = (v)=>v, Arguments = {"$VAL"}], ToText = [Function = fn_RecordToText, Arguments = {"$VAL"}]], 
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

//######fn_FunctionName## This is delimiter. Dont remove it
(f as function) => try Value.ResourceExpression(f)[Name] otherwise null

//######rec_EvalEnvironment## This is delimiter. Dont remove it
let
    Functions = lst_Functions,
    Environments = List.Accumulate(Functions, [], (state, current) => 
        try Record.AddField(state, fn_FunctionName(current[FromText][Function]), current[FromText][Function] )
        otherwise state
    ),
    Table.FromRecords = Record.AddField(Environments, "Table.FromRecords", Table.FromRecords)
in
Table.FromRecords

//######fn_Args## This is delimiter. Dont remove it
(args as list, val as any) as list => List.Transform(args, (itm)=> try if itm = "$VAL" then val else itm otherwise itm )

//######fn_ToText## This is delimiter. Dont remove it
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

//######fn_PrepText## This is delimiter. Dont remove it
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

//######fn_RecordToText## This is delimiter. Dont remove it
(rec as record) as text =>
let
    In1 = try rec otherwise [Col1 = "Normal Text", Col2 = null, Col3 = """Hello"""]
,
    Source = In1,
    Table = Record.ToTable(Source),
    FieldsToText = Table.TransformRows(Table, (row)=>
row[Name] & " = " & fn_SerializeRecursively(row[Value])
),
    RecordToText = Text.Combine({ "[", Text.Combine(FieldsToText, ","), "]"}),
    Output = RecordToText
in
    Output

//######fn_TableToText## This is delimiter. Dont remove it
(tbl as table) as text =>
let
    In1 = try tbl otherwise Table.FromRows({{"A","B"}},{"ColA", "ColB"}),
    Source = In1,
    #"List Records" = List.Transform(Table.ToRecords(  Source), fn_RecordToText),
    TableToText = Text.Combine({"Table.FromRecords({", Text.Combine(#"List Records", ","), "})"}),
    Output = TableToText
in
    Output

//######Unit Tests## This is delimiter. Dont remove it
let
    UnitTests = [
    TestText = [Value = "Test", Assert = "Pass"],
    TestDate = [Value = Date.From("20 Jun 1973"), Assert = "Pass"],
    TestDateTime = [Value = DateTime.From("1 Jan 2021 10:23:21"), Assert = "Pass"],
    TestTime = [Value = #time(10,5,1), Assert = "Pass"],
    TestInteger = [Value = 20, Assert = "Pass"],
    TestDecimal = [Value = 8.4322131, Assert = "Pass"],
    TestLogical = [Value = true, Assert = "Pass"],
    TestDuration = [Value = #duration(2, 5, 55, 20), Assert = "Pass"],
    TestBinary = [Value = Binary.FromText("1011"), Assert = "Pass"],
    TestNull = [Value = null, Assert = "Pass"],
    TestQuotation = [Value = """TestNull""", Assert = "Pass"],
    TestFunction = [Value = (val as text) as text => let out = val in out, Assert = "Fail"],
    TestTable = [Value = Table.FromRows({{"A","B"}},{"ColA", "ColB"}), Assert = "Pass"]
    ]
in
    UnitTests

//######Unit Tests Execute## This is delimiter. Dont remove it
(fn as function) as table =>
let
    fn = try fn otherwise fn_Serialize,
    UnitTests = #"Unit Tests",
    ToTable = Table.RenameColumns(Record.ToTable( UnitTests), {{"Name","Unit Test"}}),
    #"Expanded Value" = Table.ExpandRecordColumn(ToTable, "Value", {"Value", "Assert"}, {"Value", "Assert"}),
    Serialize = Table.AddColumn(#"Expanded Value", "Serialized", (rec)=> fn(rec[Value])),
    Deserialize = Table.AddColumn(Serialize, "Deserialized", (rec)=> Expression.Evaluate(rec[Serialized], rec_EvalEnvironment)),
    Check = Table.AddColumn(Deserialize, "Check", (rec)=> try if Value.Equals( rec[Value], rec[Deserialized]) then "Pass" else "Fail" otherwise "Aborted")
in
    Check

//######Unit Tests Result## This is delimiter. Dont remove it
let
   TestFunctions = {
[Name = "fn_Main", Function = fn_Main],
[Name = "fn_Serialize", Function = fn_Serialize]
},
    #"Execute Tests" = Table.FromRecords(
List.Transform(TestFunctions, (itm)=>
[Name = itm[Name], Execute = #"Unit Tests Execute"(itm[Function])]
)),
    #"Expanded Execute" = Table.ExpandTableColumn(#"Execute Tests", "Execute", {"Unit Test", "Value", "Assert", "Serialized", "Deserialized", "Check"}, {"Unit Test", "Value", "Assert", "Serialized", "Deserialized", "Check"}),
    Pass = Table.AddColumn(#"Expanded Execute", "UnitTest", (rec) =>
if Value.Equals(rec[Assert], rec[Check]) then "Pass" else "Fail"
)
in
    Pass

//######fn_Serialize## This is delimiter. Dont remove it
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
        fn_RecordToText = 
            (rec as record) as text =>
            let
                In1 = try rec otherwise [Col1 = "Normal Text", Col2 = null, Col3 = """Hello"""]
            ,
                Source = In1,
                Table = Record.ToTable(Source),
                FieldsToText = Table.TransformRows(Table, (row)=>
            row[Name] & " = " & fn_SerializeRecursively(row[Value])
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

//######fn_Compress## This is delimiter. Dont remove it
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

//######CompressTest## This is delimiter. Dont remove it
let
    Source = #"Unit Tests",
    fn_Compress = fn_Compress(fn_SerializeRecursively(Source)),
    Evaluated = Expression.Evaluate(fn_Compress, #shared)
in
    Evaluated

//######Query1## This is delimiter. Dont remove it
let
    Source = Value.Metadata(Value.Type(Csv.Document)),
    #"Documentation Examples" = Source[Documentation.Examples],
    #"Documentation Examples1" = #"Documentation Examples"{0}
in
    #"Documentation Examples1"

