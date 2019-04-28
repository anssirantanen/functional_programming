module Question where

import Data.Text
import Data.Char
import Data.Foldable
import Data.Maybe
import Data.Functor






data QuestionType = Location | LocationWhere deriving Show

data Question = Question { questionType:: QuestionType,
                            actor:: Text,
                            target :: Text
} deriving Show
locationKeys = [ pack "is"]
locationWhereKeys = [pack "where"]

locationMatcher :: Text -> [Text] -> Maybe Text 
locationMatcher rawString keys= 
    let locations = Prelude.map (\t -> parseLocation t rawString) keys
      in asum locations

deriveLocationType :: Text -> Maybe(QuestionType, Text)
deriveLocationType rawStr = 
    case locationMatcher rawStr locationKeys of
        Just x -> Just(Location,x)
        Nothing -> fmap(\e -> (LocationWhere,e)) (locationMatcher rawStr locationWhereKeys)
deriveQuestion :: Text -> Maybe Question
deriveQuestion rawStr = 
    case deriveLocationType rawStr of 
        Just (t, str) ->
            let (actor, target) = locationActor str
            in Just (Question t actor(locationTarget target))
        Nothing -> Nothing
testQ str = deriveQuestion str 
locationActor rawStrs =
    breakOn (pack " ") (Data.Text.dropWhile (==' ') rawStrs) 

locationTarget rawStr =
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

