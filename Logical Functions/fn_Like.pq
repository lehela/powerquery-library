let func = (phrase as text, pattern as text, optional wildcard as text) => 
   let
      CheckPhrase = try phrase otherwise "Do a Check",
      Token = try pattern otherwise "*h*ck*",
      WildcardDelim = try if wildcard = null or wildcard = "" then "*" else wildcard otherwise "*",

      //Split Token up into a list using % as a delimiter
      TokenList = Text.Split(Token, WildcardDelim),
      //if the first character in the Token is %
      //then the first item in the list is an empty string
      StartsWithWc = (List.First(TokenList)=""),
      //if the last character in the Token is %
      //then the last item in the list is an empty string
      EndsWithWc = (List.Last(TokenList)=""),
      //if the first character is not %
      //then we have to match the first string in the Token
      //with the opening characters of the CheckPhrase
      StartsTest = if (StartsWithWc=false) 
         then Text.StartsWith(CheckPhrase, List.First(TokenList)) 
         else true,
      //if the last item is not %
      //then we have to match the final string in the Token
      //with the final characters of the CheckPhrase
      EndsText = if (EndsWithWc=false) 
         then Text.EndsWith(CheckPhrase, List.Last(TokenList)) 
         else true,
      //now we also need to check that each string in the Token appears 
      //in the correct order in the CheckPhrase
      //and to do this we need to declare a function CheckPhraseFind
      CheckPhraseFind = (CheckPhrase as text, SearchString as list) =>
      let
      //does the first string in the Token appear in the CheckPhrase?
      StringPos = Text.PositionOf(CheckPhrase, SearchString{0}, Occurrence.First),
      CheckPhraseFindOutput = 
                  if
                  //if string not find then return false 
                  (StringPos=-1) 
                  then false 
                  else if
                  //we have found the string in the Token, and
                  //if this is the last string in the Token, return true
                  List.Count(SearchString)=1
                  then true
                  else
                  //if it isn't the last string in the Token
                  //test the next string in the Token by removing
                  //the first string from the Token list
                  //and all text up to and including the string we have found in the CheckPhrase
                  (true and
                  @CheckPhraseFind(
                  Text.RemoveRange(CheckPhrase, 0, StringPos + Text.Length(SearchString{0})),
                  List.RemoveRange(SearchString, 0, 1)))
      in
         CheckPhraseFindOutput,
      //return true if we have passed all tests    
      Output = StartsTest and EndsText and CheckPhraseFind(CheckPhrase, TokenList) 
   in
    Output
    ,
    documentation = [
    Documentation.Name =  " fn_Like ",
    Documentation.Description = " Like operator similar to SQL",
    Documentation.LongDescription = " Returns boolean true if the phrase matches the pattern ",
    Documentation.Category = " Other ",
    Documentation.Source = " https://blog.crossjoin.co.uk/2014/05/27/implementing-a-basic-likewildcard-search-function-in-power-query/ ",
    Documentation.Version = " 1.0 ",
    Documentation.Author = " Chris Webb ",
    Documentation.Examples = {
       [    Description  = " Match ",
            Code   = " fn_Like(""Example sentence"", ""*ente*"")  ",
            Result = " true "]
      ,
       [    Description  = "  No Match ",
            Code   = " fn_Like(""Example sentence"", ""%ante%"", ""%"")  ",
            Result = " true "]     
      }]
  
 in  
    Value.ReplaceType(func, Value.ReplaceMetadata(Value.Type(func), documentation))