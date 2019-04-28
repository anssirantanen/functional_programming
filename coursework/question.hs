module Question where

import Data.Text
import Data.Char
import Data.Foldable
import Data.Maybe
import Data.Functor






data QuestionType = Location | LocationWhere | Count deriving Show

data Question = Question { questionType:: QuestionType,
                            actor:: Text,
                            target :: Text
} deriving Show
locationKeys = [ pack "is"]
locationWhereKeys = [pack "where"]
countKeys = [pack "how many objects is"]

typeMatcher :: Text -> [Text] -> Maybe Text 
typeMatcher rawString keys= 
    let locations = Prelude.map (\t -> parseLocation t rawString) keys
      in asum locations

deriveLocationType :: Text -> Maybe(QuestionType, Text)
deriveLocationType rawStr = 
    case typeMatcher rawStr locationKeys of
        Just x -> Just(Location,x)
        Nothing -> fmap(\e -> (LocationWhere,e)) (typeMatcher rawStr locationWhereKeys)
deriveCountType :: Text -> Maybe(QuestionType, Text)
deriveCountType rawStr = 
    fmap(\e -> (Count, e))(typeMatcher rawStr countKeys)
deriveQuestion :: Text -> Maybe Question
deriveQuestion rawStr = 
    case deriveLocationType rawStr of 
        Just (t, str) ->
            let (actor, target) = parseActor str
            in Just (Question t actor(parseTarget target))
        Nothing -> case deriveCountType rawStr of 
            Just (t, str) -> 
                let (actor, target) = parseActor str
                in Just (Question t actor(parseTarget target))
            Nothing -> Nothing
parseActor rawStrs =
    breakOn (pack " ") (Data.Text.dropWhile (==' ') rawStrs) 

parseTarget rawStr =
    cleanText rawStr

parseLocation:: Text -> Text -> Maybe Text
parseLocation elem rawString =
    case  isPrefixOf elem rawString of
    True ->  stripPrefix elem rawString
    False ->  Nothing

    
cleanText textPart =
   (Data.Text.filter (\t ->  not (isSpace t)) 
   . replace (pack "?") (pack "")
   . replace (pack "in") (pack "")
   . replace (pack "to") (pack "") 
   . replace (pack "the") (pack ""))(textPart)

