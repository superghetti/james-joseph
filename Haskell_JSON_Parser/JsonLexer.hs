module JsonLexer where
import System.IO
import Data.Char

{-
  This module defines records used to keep track of lexical analysis of JSON files.
-}

-- position information for a file, helpful for errors when tokenizing
data FilePos = FilePos { name :: String, line :: Int, col :: Int } deriving (Eq, Show)

-- token types for JSON
data TokenType =
    LBrace
  | RBrace
  | Colon
  | LBracket
  | RBracket
  | StringTok (String)
  | FloatTok (Double)
  | TrueTok
  | FalseTok
  | NullTok
  | Comma deriving (Eq, Show)


-- returns true or false if the token is a 'value' type, i.e., it contains or is a simple value
isValueTokenType t =
  case t of
    StringTok _ -> True
    FloatTok _ -> True
    TrueTok -> True
    FalseTok -> True
    NullTok -> True
    _ -> False

-- token consists of a token type and file position
data Token = Token (TokenType) (FilePos) deriving (Show, Eq)

isValueToken t =
  case t of
    Token ty _ -> isValueTokenType ty

{-  functions that affect the FilePos struct -}
-- adds a newline to our file position
addNewline (FilePos { name = n, line = l, col = c }) =
  FilePos { name = n, line = (l + 1), col = 0}

-- adds a char to our current file position
addChar (FilePos { name = n, line = l, col = c }) =
  FilePos { name = n, line = l, col = (c + 1)}

-- adds several characters to our current file position
addRange (FilePos { name = nm, line = l, col = c}) n =
  FilePos { name = nm, line = l, col = (c + n) }

-- creates a new, clean file position
initPos n l c =
  FilePos { name = n, line = l, col = c }

-- this returns a function
makeTokenizer :: Handle -> FilePos -> IO Token
makeTokenizer handle = (\fpos -> getNextToken handle fpos)


{-
  This is the main function that gets the next token off the file. When called,
  it returns the next token it can, or an error. Assuming you're keeping track of
  the file properly, thie file position may be accurate!
-}
getNextToken :: Handle -> FilePos -> IO Token
getNextToken fHandle fPos = do
  -- grab the character
  c <- hGetChar fHandle
  -- now see what it is
  case c of
    -- whitespace
    '\n' -> getNextToken fHandle (addNewline fPos)
    ' ' -> getNextToken fHandle (addChar fPos)
    '{' -> return (Token LBrace (addChar fPos))
    '}' -> return (Token RBrace (addChar fPos))
    '[' -> return (Token LBracket (addChar fPos))
    ']' -> return (Token RBracket (addChar fPos))
    ',' -> return (Token Comma (addChar fPos))
    ':' -> return (Token Colon (addChar fPos))
    '"' -> recognizeString fHandle (addChar fPos)
    't' -> recognizeTrue fHandle (addChar fPos)
    'f' -> recognizeFalse fHandle (addChar fPos)
    'n' -> recognizeNull fHandle (addChar fPos)
    '-' -> recognizeNumber fHandle (addChar fPos) c
    _ -> if isDigit c
            then recognizeNumber fHandle (addChar fPos) c
            else
              error ("Unexpected token " ++ [c] ++ ", " ++ (show fPos))



-- recognizeString takes a file handle, a file position, and tries to determine
-- if there's a string. It collects the interior of the string until it finds
-- a non-escaped quote.
recognizeString fHandle fPos =
  let
    helper fPos acc = do
      c <- hGetChar fHandle
      case c of
        '"' -> return (Token (StringTok (acc)) (addChar fPos))
        '\\' -> do
          -- extract the escaped characters
          (res, newPos) <- recognizeEscape fHandle fPos
          -- continue looking for the rest of the string
          helper newPos (acc ++ res)
        _ -> helper (addChar fPos) (acc ++ (c:[]))
  in
    helper fPos ""




-- returns true or false if the character following a \ is a valid
-- escape character
isValidEscape :: Char -> Bool
isValidEscape c =
  case c of
    '"' -> True
    '\\' -> True
    '/' -> True
    'b' -> True
    'f' -> True
    'n' -> True
    'r' -> True
    't' -> True
    'u' -> True
    _ -> False


-- completes the escape recognition
recognizeEscape fHandle fPos = do
  c <- hGetChar fHandle
  if isValidEscape c
     then if c == 'u'
             then recognizeUnicodeEscape fHandle fPos
             else return ('\\':c:[], (addRange fPos 2))
     else error ("Invalid escape " ++ ('\\':c:[]) ++ ", " ++ (show fPos))


-- this reads the next 4 digits to determine if they're hex
recognizeUnicodeEscape fHandle fPos = do
  c1 <- hGetChar fHandle
  c2 <- hGetChar fHandle
  c3 <- hGetChar fHandle
  c4 <- hGetChar fHandle

  let escape = '\\':'u':c1:c2:c3:c4:[]
  if ((isHexDigit c1) && (isHexDigit c2) && (isHexDigit c3) && (isHexDigit c4))
     then return (escape, (addRange fPos 6))
     else error ("Invalid unicode escape " ++ escape ++ ", " ++ (show fPos))


-- tries to recognize the token for true, basically it reads the next three chars
recognizeTrue fHandle fPos = do
  c2 <- hGetChar fHandle
  c3 <- hGetChar fHandle
  c4 <- hGetChar fHandle

  if (c2 == 'r' && c3 == 'u' && c4 == 'e')
    then return (Token TrueTok (addRange fPos 4))
    else error ("Expecing 'true', got: " ++ ('t':c2:c3:c4:[]) ++ ", " ++ (show fPos))

-- tries to recognize the token for false
recognizeFalse fHandle fPos = do
  c2 <- hGetChar fHandle
  c3 <- hGetChar fHandle
  c4 <- hGetChar fHandle
  c5 <- hGetChar fHandle

  if (c2 == 'a' && c3 == 'l' && c4 == 's' && c5 == 'e')
    then return (Token FalseTok (addRange fPos 4))
    else error ("Expecing 'false', got: " ++ ('f':c2:c3:c4:c5:[]) ++ ", " ++ (show fPos))

-- tries to recognize the token for null
recognizeNull fHandle fPos = do
  c2 <- hGetChar fHandle
  c3 <- hGetChar fHandle
  c4 <- hGetChar fHandle

  if (c2 == 'u' && c3 == 'l' && c4 == 'l')
    then return (Token NullTok (addRange fPos 4))
    else error ("Expecing 'null', got: " ++ ('n':c2:c3:c4:[]) ++ ", " ++ (show fPos))


-- tries to recognize a valid floating point number, this is probably the
-- most complicated part of the tokenizer--a number can have all sorts of things,
-- like decimals, negation, and exponents!
recognizeNumber fHandle fPos c = do
  let
    helper fPos acc = do
      -- get the next character
      c <- hLookAhead fHandle
      if isDigit c
         then do
            ch <- hGetChar fHandle
            helper (addChar fPos) (ch:acc)
         else
           case c of
             '.' -> do
               ch <- hGetChar fHandle
               recognizeNumPart2 fHandle (addChar fPos) (ch:acc)
             'e' -> do
               ch <- hGetChar fHandle
               recognizeExponent fHandle (addChar fPos) (ch:acc)
             'E' -> do
               ch <- hGetChar fHandle
               recognizeExponent fHandle (addChar fPos) (ch:acc)
             _ -> return $ Token (FloatTok (read (reverse acc) :: Double)) fPos
  helper fPos [c]


-- continues recognizing the number after the decimal point
recognizeNumPart2 fHandle fPos acc = do
  c <- hLookAhead fHandle
  if isDigit c
     then do
       ch <- hGetChar fHandle
       recognizeNumPart2 fHandle (addChar fPos) (ch:acc)
     else
       case c of
         'e' -> do
           ch <- hGetChar fHandle
           recognizeExponent fHandle (addChar fPos) (ch:acc)
         'E' -> do
           ch <- hGetChar fHandle
           recognizeExponent fHandle (addChar fPos) (ch:acc)
         _ -> return $ Token (FloatTok (read (reverse acc) :: Double)) fPos


-- reconizes an exponent notation in the number
recognizeExponent fHandle fPos acc = do
  c <- hLookAhead fHandle
  if isDigit c
     -- keep adding digits
     then do
        ch <- hGetChar fHandle
        recognizeExponent fHandle (addChar fPos) (ch:acc)
     else if c == '-' || c == '+'
             then do
               ch <- hGetChar fHandle
               recognizeFinalDigits fHandle (addChar fPos) (ch:acc)
             else return $ Token (FloatTok (read (reverse acc) :: Double)) fPos


-- and finally, recognizes the digits of the exponent
recognizeFinalDigits fHandle fPos acc = do
  c <- hLookAhead fHandle
  if isDigit c
    -- keep adding digits only
    then do
      ch <- hGetChar fHandle
      recognizeFinalDigits fHandle (addChar fPos) (ch:acc)
    else
      return $ Token (FloatTok (read (reverse acc) :: Double)) fPos
