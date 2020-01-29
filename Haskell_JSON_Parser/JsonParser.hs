module JsonParser where
import JsonLexer

{- a datatype representing Json stuff -}
data Json =
    TrueVal
  | FalseVal
  | NullVal
  | StrVal (String)
  | NumVal (Double) 
  | ObjVal [(String, Json)]
  | Array [Json] deriving (Eq, Show)



{-
  This function parses Json using a hand-written top-down parser; Json is nice like this.
  The idea is that it's an IO Json function and will return the value that it parses, but
  truthfully, it should always be an ObjVal type. Note that we take a tokenizer which is
  a function that when called with a file position object will give us the next token.
-}
parseObject :: (JsonLexer.FilePos -> IO Token) -> JsonLexer.FilePos -> IO Json
parseObject tokenizer fPos = do
  -- if I'm expecting a true value, I should get a true token
  token <- tokenizer fPos
  case token of
    -- objects must begin with an LBrace, and this is why it's an LL(1) parser 
    Token LBrace pos -> parseList tokenizer pos []
    _ -> error "expected LBrace"



-- objects in JSON are made up of lists of string/value pairs, so we create
-- an ObjVal for every list we find, and we know where exactly these will be
parseList :: (JsonLexer.FilePos -> IO Token) -> JsonLexer.FilePos -> [(String, Json)] -> IO Json
parseList tokenizer fPos acc = do
  token <- tokenizer fPos
  case token of
    -- the base case, we get to a closing brace, so we return the accumulator
    Token RBrace _ -> return $ ObjVal (reverse acc)
    -- we expect a string though because an object usually has a list of pairs
    Token (StringTok name) pos -> do
      -- retrieve the pair
      pair <- parsePair tokenizer pos name
      -- then get the next token
      nextTok <- tokenizer pos
      case nextTok of
        Token Comma pos -> parseList tokenizer pos (pair:acc)
        Token RBrace _ -> return $ ObjVal (reverse (pair:acc))
        Token RBracket _ -> return $ ObjVal (reverse (pair:acc))
        Token tok pos -> error ("Expected a comma or colon at " ++ (show pos) ++ ", saw " ++ (show tok) ++ " instead.")

    -- if we're parsing an object it has to either end with a brace, or have a list
    -- that starts with a string token, so if we hit this case, it's an error
    Token tok pos -> error ("(2) Expected a comma or right brace at " ++ (show pos) ++ ", saw " ++ (show tok) ++ " instead, see: " ++ (show token))
        


{-
  parseJson parses a Json value and returns it. The first argument is of course the
  tokenizer, the 2nd is the file position and we return a Json value. 
-}
parseJson :: (JsonLexer.FilePos -> IO Token) -> JsonLexer.FilePos -> IO Json
-- parses a value and returns it, it's like an IO (String, Json)
parseJson tokenizer fPos = do
  val <- tokenizer fPos
  case val of
    -- this means it's another object, so parse the list again, which returns an object
    Token LBrace pos -> parseList tokenizer pos []
    Token LBracket pos -> parseArray tokenizer pos []
    Token tty pos ->
      if isValueTokenType tty
         then return (tokenTypeToVal tty)
         else error ("Expected a value token or start of a object at " ++ (show pos) ++ ", saw " ++ (show val) ++ " instead.")





{-
  parsePair parses a pair and returns it as a Haskell pair. We assume we were
  called after discovering the "key" or "field" value, which is the 3rd argument.
  The 1st argument is our tokenizer, the 2nd our file position, the 3rd is the field
  name, and we return an IO (String, Json) pair.
-}
parsePair :: (JsonLexer.FilePos -> IO Token) -> JsonLexer.FilePos -> String -> IO (String, Json)
-- we parse a pair by looking for the colon first, then we try to parse the value
parsePair tokenizer fPos name = do
  -- here we must consume a comma first
  colon <- tokenizer fPos
  case colon of
    Token Colon pos -> do
      val <- parseJson tokenizer pos
      -- return that pair
      return (name, val)
      
    -- hey, an error message if it's any other kind of token
    Token tok pos -> error ("Expected a colon at " ++ (show pos) ++ ", saw " ++ (show tok) ++ " instead.")



{-
  parseArray takes a tokenizer, file position, and an accumulator to build up a Json array.
-}
parseArray :: (JsonLexer.FilePos -> IO Token) -> JsonLexer.FilePos -> [Json] -> IO Json
parseArray tokenizer fPos acc = do
  valTok <- tokenizer fPos
  case valTok of
    -- end of the array, so the base case
    Token RBracket _ -> return (Array (reverse acc))
    -- could be an object inside the array
    Token LBrace pos -> do
      -- try to parse the object, it should return an ObjVal
      val <- parseList tokenizer pos []
      endOrContinueArray tokenizer pos (val:acc)
    -- could be another array
    Token LBracket pos -> do
      val <- parseArray tokenizer pos []
      endOrContinueArray tokenizer pos (val:acc)
    -- or a data value
    Token tty pos ->
      if isValueTokenType tty
         then do
            let val = tokenTypeToVal tty
            endOrContinueArray tokenizer pos (val:acc)
         else error ("Expected a comma or right bracket at " ++ (show pos) ++ ", saw " ++ (show tty) ++ " instead.")


{-
  When an array is about to end, what comes next depends on the next token (this is the lookahead).
  If it's a bracket, the array is done, but if it's a comma, we continue the array. 
-}
endOrContinueArray tokenizer pos acc = do
  -- grab the next token
  end <- tokenizer pos
  case end of
    -- base case, we end the array
    Token RBracket pos -> return $ Array (reverse acc)
    -- this means continue...
    Token Comma pos -> parseArray tokenizer pos acc
    _ -> error "Expected comma or right bracket to close array"



{-
  tokenTypeToVal: converts a token to a Json value, but this only works for
  those tokens which are actually "value" tokens, like null and false--objects and
  arrays take quite a bit more work.
-}
tokenTypeToVal :: TokenType -> Json
tokenTypeToVal ttype =
  case ttype of
    TrueTok -> TrueVal
    FalseTok -> FalseVal
    NullTok -> NullVal
    StringTok str -> StrVal str
    FloatTok num -> NumVal num
