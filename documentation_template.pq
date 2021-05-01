let
    func = (text1 as text, optional text2 as text) as list =>
        let
            Output = "Dummy output"
        in
            Output,
    FunctionDoc = 
        [
        Documentation.Name = "fn_documentation_demo",
        Documentation.Description = 
                "This function simply demos how custom functions can be documented more easily. The following variables can be populated, and are then applied by the <code>fn_createDocType</code> function:<br>" &
                " - <code>FunctionDoc</code> contains the documentation record for the function itself<br>" &
                " - <code>ParameterDoc</code> contains a list of optional parameter documentation records, e.g. to limit allowed input values." & 
                "</p>The <code>fn_createDocType</code> function dynamically updates only explicitly listed parameters, so there is no need to list parameters which do not need any documentation or allowed value limits." &
                "</p>Moreover, here are some supported HTML formattings that can be used in the <code>Documentation.Description</code> field:</br>" &
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
    ,
    fn_createDocType = (fn as function, fn_doc as record, optional param_doc as list) as type => 
        let
            funcType = Value.Type(fn),
            funcParams = Type.FunctionParameters(funcType),
            funcParamsDoc = Record.TransformFields(funcParams,
                    try List.Transform(param_doc, (doc)=> {doc[Name], (parType)=> Value.ReplaceMetadata(parType, doc)})
                    otherwise {}
                )
        in
            Type.ForFunction(
                [
                    ReturnType = Type.FunctionReturn(funcType),
                    Parameters = funcParamsDoc
                ]
                , Type.FunctionRequiredParameters(funcType)
            ) meta fn_doc




in
    Value.ReplaceType(func, fn_createDocType(func, FunctionDoc, ParametersDoc))