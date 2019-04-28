module Item where
import Statement
import Data.Text
import Data.Maybe


data OperationType = Pick | Drop deriving (Show, Eq)

data Item  = Item {
    holder :: Text, 
    action :: OperationType,
    target :: Text
} deriving Show
positiveItems = [pack "took", pack "got", pack "pickedup"]
negativeItems = [pack "discarded"]

isPosAction:: Text -> Bool 
isPosAction statement =
    Prelude.any (\e -> (Data.Text.count statement e)> 0) positiveItems
isNegAction :: Text -> Bool
isNegAction statement = 
    Prelude.any (\e -> (Data.Text.count statement e)> 0) negativeItems

parseItem :: Statement -> Maybe Item
parseItem statement
    | isPosAction (Statement.action statement) = Just (Item (Statement.actor statement) Pick (Statement.target statement)) 
    | isNegAction (Statement.action statement) = Just (Item (Statement.actor statement) Drop (Statement.target statement)) 
    | otherwise = Nothing

parseItems :: [Statement] -> [Item]
parseItems statements =
 catMaybes (Prelude.map parseItem statements)
    
