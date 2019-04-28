module Main (main) where

import System.Environment 
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as IOT
import Statement
import Location
import Question
import Extractor
import Item

main = do
    [x] <- getArgs
    text <- IOT.readFile x
    let textLines = Text.lines text  
    let statements = map buildStatement textLines 
    let locations = parseLocations statements
    let items = parseItems statements
    readQuestion locations items

readQuestion locations items  = do 
    putStrLn "ask a question"
    question <- getLine
    let statementM = deriveQuestion (toText question)
        is = fmap(\q -> extractQuestion q locations items) statementM
    putStrLn(show statementM)
    putStrLn (unpackN is)
    if question == "" then putStrLn "Exit" else readQuestion locations items

toText str = 
    Text.toLower (Text.pack str)

unpackN :: Maybe String -> String
unpackN (Just s) = s
unpackN Nothing = "dont know"
    
