let
    // This is just a dummy function to demo the documentation with. It does nothing..
    func = (text1 as text, optional text2 as text) as list =>
        let
            Output = "Dummy output"
        in
            Output,
    // This is the record that holds the Documentation for the function itself
    // It shows that a number of HTML tags can be used in the Description field
    FunctionDoc = 
        [
        Documentation.Name = "fn_documentation_demo",
        Documentation.Description = 
                "This function simply demos how custom functions can be documented more easily. The following variables can be populated, and are then applied by the <code>fn_applyDocumentation</code> function:<br>" &
                " - <code>FunctionDoc</code> contains the documentation record for the function itself<br>" &
                " - <code>ParameterDoc</code> contains a list of optional parameter documentation records, e.g. to limit allowed input values." & 
                "</p>The <code>fn_applyDocumentation</code> function dynamically updates only explicitly listed parameters, so there is no need to list parameters which do not need any documentation or allowed value limits." &
                "</p>Moreover, here are some supported HTML formattings that can be used in the <code>Documentation.Description</code> field:</br>" &
                "<ul>" &
                "<li>&lt;/p&gt;: Paragraph break" &
                "<li>&lt;br&gt;: Normal line break" &
                "<li>&lt;ul&gt;&lt;li&gt;;Unordered list items&lt;/li&gt;&lt;/ul&gt;;</li>" &
                "<li>&lt;i&gt;<i>Italic script</i>&lt;/i&gt;</li>" &
                "<li>&lt;b&gt;<b>Bold font</b>&lt;/b&gt;</li>" &
                "<li>&lt;code&gt;<code>Technical script</code>&lt;/code&gt;</li>" 
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
    // This is an array of Parameter documentation records. These are dynamically applied to existing function parameters by the fn_applyDocumentation function.
    // Note that FieldDescription and the functions' documentation cannot be shown at the same time, which seems to be a performance related bug.
    ParametersDoc = {
            [
            Name = "text2",
            Documentation.FieldCaption = "Pick a value",
            Documentation.FieldDescription = "This is not getting displayed, unless the function type documentation itself is removed.",
            Documentation.AllowedValues = {"Value 1", "Value 2", "Value 3"}
            ]
            }
    ,
    // This function is dynamically applying documentation records to an input function
    fn_applyDocumentation = (fn as function, fn_doc as record, optional param_doc as list) as function => 
        let
            // The functions' original type without documentation applied
            funcType = Value.Type(fn),
            
            // Apply parameter documentation to the respective parameters of the input function
            funcParamDoc = Record.TransformFields(Type.FunctionParameters(funcType),
                    try List.Transform(param_doc, (doc)=> {doc[Name], (parType)=> Value.ReplaceMetadata(parType, doc)})
                    otherwise {}
                ),
            // Create a new function type with all documentation applied
            funcTypeAppliedDoc = Type.ForFunction(
                [
                    ReturnType = Type.FunctionReturn(funcType),
                    Parameters = funcParamDoc
                ]
                , Type.FunctionRequiredParameters(funcType)
            ) meta fn_doc
        in 
            // Apply documented function type to input function
            Value.ReplaceType(fn, funcTypeAppliedDoc)

in
    fn_applyDocumentation(func, FunctionDoc, ParametersDoc)