let
    func = (text1 as text, optional text2 as text) as list =>
        let
            Output = "Dummy output"
        in
            Output,
    docType = 
        let
            funcType = Value.Type(func),
            funcParams = Type.FunctionParameters(funcType),
            funcParamsDoc = Record.TransformFields(funcParams,
                    List.Transform(ParametersDoc, (doc)=> {doc[Name], (parType)=> Value.ReplaceMetadata(parType, doc)})
                )
        in
            Type.ForFunction(
                [
                    ReturnType = Type.FunctionReturn(funcType),
                    Parameters = funcParamsDoc
                ]
                , Type.FunctionRequiredParameters(funcType)
            ) meta FunctionDoc
    ,
    FunctionDoc = 
        [
        Documentation.Name = "fn_documentation_demo",
        Documentation.Description = 
                "This function simply demos how custom functions can be documented.</p>" &
                "The variable <code>FunctionDoc</code> contains the documentation record for the function itself<br>" &
                "The variable <code>ParameterDoc</code> contains a list of parameter documentation records. Undocumented parameters do not need to be listed, as the <code>docType</code> expression applies the parameters dynamically onto the function. " &
                "</p>Moreover, here are some supported HTML formattings that can be used in the <code>Documentation.Description</code>:</br>" &
                "<ul>" &
                "<li>&lt;/p&gt: Paragraph break" &
                "<li>&lt;br&gt: Normal line break" &
                "<li>&lt;ul&gt&lt;li&gt;Unordered list items&lt;/li&gt&lt;/ul&gt;</li>" &
                "<li>&lt;i&gt<i>Italic script</i>&lt;/i&gt</li>" &
                "<li>&lt;b&gt<b>Bold font</b>&lt;/b&gt</li>" &
                "<li>&lt;code&gt<code>Technical script</code>&lt;/code&gt</li>" 
                ,
        Documentation.Examples = 
            {
            [
                Description = "1",
                Code = "Some command to be evaluated.",
                Result = "1234"
            ]
            }
        ],
    ParametersDoc = {
            [
            Name = "text2",
            Documentation.FieldCaption = "Pick a value",
            Documentation.FieldDescription = "This is not getting displayed, unless the function type documentation itself is removed.",
            Documentation.AllowedValues = {"Value 1", "Value 2", "Value 3"}
            ]
            }


in
    Value.ReplaceType(func, docType)