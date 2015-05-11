module PardonMyFrench.CorrectMistakes where

import PardonMyFrench.FrenchParser
import Data.List.Split
import Data.List
 
logPath = "events.log" 

logJson :: String => String => String
logJson eventType sentence = "{ \"type\": \"" ++ eventType ++ "\", \"sentence\": \"" ++ sentence ++ "\"}\n"

successLog :: String => String
successLog sentence = logJson "success" sentence

errorLog :: String => String
errorLog sentence = logJson "error" sentence

beginSession = "+++\n"

logTries :: [String] => IO ()
logTries tries = do
   appendFile logPath beginSession
   mapM_ (appendFile logPath) tries


askTillRight :: [String] => Sentence => IO [String]
askTillRight tries sentence = do
   let wSentence = (wrongSentence sentence)
   putStrLn $ show (length tries) ++ ">" ++ (intercalate " " wSentence)
   input <- getLine
   if (splitOn " " input) == (correctSentence sentence) then  do
     return $ tries ++ [successLog input]
   else do
     askTillRight (tries ++ [(errorLog input)]) sentence

