module Extractor where 
import Data.Text
import Question 
import Location
import Item
import Data.List
import Data.Maybe
import Data.Functor

isLocation::Question -> [Location.Location] -> Maybe Bool
isLocation question locations = 
    let locationM = Data.List.find (\l -> (Question.actor question)==(Location.actor l))( Data.List.reverse locations)
    in fmap (\m ->  (Question.target question) == (Location.target m)) locationM

findItemActor :: Question -> [Item.Item] -> Maybe Text
findItemActor question items =
    fmap (\e-> Item.holder e)(Data.List.find (\e-> Question.target question == Item.target e) items)

findActorLocation :: Question -> [Location.Location] -> Maybe Text
findActorLocation question locations =
   fmap (\e -> Location.target e)(Data.List.find (\e -> Question.actor question == Location.actor e) (Data.List.reverse locations))

chainedActorLocation :: Question -> [Location.Location] ->[Item.Item] -> Maybe Text
chainedActorLocation question locations items = 
    case findActorLocation question locations of
        Just x -> Just x
        _ -> (findItemActor question items) >>= (\a -> findActorLocation (Question LocationWhere a Data.Text.empty) locations)

extractQuestion :: Question -> [Location] -> [Item.Item]-> String
extractQuestion question locations items = 
    case Question.questionType question of
        Question.Location -> unpackMaybeB(isLocation question locations)
        Question.LocationWhere -> unpackMaybeWhere(chainedActorLocation question locations items)

unpackMaybeB:: Maybe Bool -> String
unpackMaybeB m = case m of 
    Just True -> "Yes"
    Just False -> "No"
    Nothing -> "Maybe"
unpackMaybeWhere:: Maybe Text -> String
unpackMaybeWhere (Just s) = unpack s
unpackMaybeWhere Nothing = "don't know"