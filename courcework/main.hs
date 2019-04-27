module Main (main) where

import System.Environment 
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as IOT
import Statement
import Location
import Question
import Extractor

main = do
    [x] <- getArgs
    text <- IOT.readFile x
    let textLines = Text.lines text  
    let statements = map buildStatement textLines 
    let locations = parseLocations statements
    readQuestion locations

readQuestion locations  = do 
    putStrLn "ask a question"
    question <- getLine
    let statementM = buildLocation (toText question)
        is = statementM >>= (\q -> isLocation q locations) 
    putStrLn (unpackMaybeB is)
    if question == "" then putStrLn "Exit" else readQuestion locations

toText str = 
    Text.toLower (Text.pack str)

unpackMaybeB:: Maybe Bool -> String
unpackMaybeB m = case m of 
    Just True -> "Yes"
    Just False -> "No"
    Nothing -> "Maybe"
    
    
