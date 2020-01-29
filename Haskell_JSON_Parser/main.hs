import A4b
import JsonParser
import JsonLexer
import System.IO
import Control.Monad
import Data.Map
import System.Environment

--takes arg, uses to open file and parse to objval, then evaluates and prints to console
main = do
  args <- getArgs
  when (length args > 0) $ do
    let filename = (head args)
    obj <- parse filename
    putStrLn (printDiseases (increasingIncidents obj))
