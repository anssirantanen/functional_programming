module Extractor where 
import Data.Text
import Question 
import Location
import Data.List
import Data.Maybe
import Data.Functor

isLocation::Question -> [Location.Location] -> Maybe Bool
isLocation question locations = 
    let locationM = Data.List.find (\l -> (Question.actor question)==(Location.actor l))( Data.List.reverse locations)
    in fmap (\m ->  (Question.target question) == (Location.target m)) locationM
