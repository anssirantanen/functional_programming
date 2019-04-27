module Question where

import Data.Text
import Data.Char
import Data.Foldable
import Data.Maybe
import Data.Functor
locationKeys = [ pack "is"]

data Location = Location deriving Show
--data QuestionType = Location

data Question = Question { questionType:: Location,
                            actor:: Text,
                            target :: Text
} deriving Show
--buildQuestion :: Text => Question
--buildQuestion rawString = 
--    let question = breakOn (pack " ") rawString
    
buildLocation str  =
    let locationM  = locationMaybe str
        actorM = fmap locationActor locationM
    in fmap (\(actor, raw)-> Question Location actor (locationTarget raw)) actorM
locationMaybe :: Text -> Maybe Text
locationMaybe rawString = 
  let locations = Prelude.map (\t -> parseLocation t rawString) locationKeys
    in asum locations

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

