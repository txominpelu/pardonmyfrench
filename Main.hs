--
-- Copyright (c) 2006 Don Stewart - http://www.cse.unsw.edu.au/~dons/
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--
import System.Environment
import PardonMyFrench.FrenchParser
import Text.Parsec
import Data.List.Split
import Data.Either
import Data.List
 

correctWord :: Word => String
correctWord (Correct x) = x
correctWord (ErroredWord w c) = c

wrongWord :: Word => String
wrongWord (Correct x) = x
wrongWord (ErroredWord w c) = w

correctSentence :: Sentence => [String]
correctSentence (Sentence s) = map correctWord s

wrongSentence :: Sentence => [String]
wrongSentence (Sentence s) = map wrongWord s

askQuestion :: Sentence => IO ()
askQuestion sentence = do
   let wSentence = (wrongSentence sentence)
   putStrLn $ "?> " ++ (intercalate " " wSentence)
   putStrLn "$> "
   input <- getLine
   putStrLn $ show $ (splitOn " " input) == (correctSentence sentence)


-- | 'main' runs the main program
main :: IO ()
main = do
   args <- getArgs
   content <- readFile (args !! 0) 
   let linesOfFiles = lines content
       parsed = map (\l -> parse sentence "" l)  linesOfFiles
   mapM_ askQuestion (rights parsed)
       
 
