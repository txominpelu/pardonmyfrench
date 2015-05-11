--
-- Copyright (c) 2006 Don Stewart - http://www.cse.unsw.edu.au/~dons/
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--
import System.Environment
import PardonMyFrench.FrenchParser
import PardonMyFrench.CorrectMistakes
import Text.Parsec
import Data.Either
import Data.List
import Data.Random
import Data.Random.List
import Data.Random.Source.DevRandom
import qualified Data.Random.Extras

errorsFile = "errors.csv"
batchQuestionsSize = 3

readMistakes :: String => IO [Sentence] 
readMistakes file = do 
   content <- readFile file 
   let linesOfFiles = lines content
       parsed = map (\l -> parse sentence "" l)  linesOfFiles
   return (rights parsed)

correctMistakes :: [Sentence] => IO ()
correctMistakes mistakes = do
   tries <- mapM (askTillRight []) mistakes 
   mapM logTries tries
   let totalNumTries = foldl1' (+) (map length tries)
   putStrLn $ "Total tries: (" ++ show totalNumTries  ++ "/" ++ show (length mistakes) ++ ")"


repl :: [Sentence] => String => IO () 
repl mistakes "mistakes" = do 
   batchMistakes <- runRVar (Data.Random.Extras.sample batchQuestionsSize mistakes) DevRandom
   correctMistakes batchMistakes
   repl mistakes "main"
repl mistakes "quit" = do 
   putStrLn "Bye"
repl mistakes any = do
   putStrLn "$>"
   line <- getLine
   repl mistakes line
  
-- | 'main' runs the main program
main :: IO ()
main = do
   mistakes <- readMistakes errorsFile
   repl mistakes "main"

       
 
