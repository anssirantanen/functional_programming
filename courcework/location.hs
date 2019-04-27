module Location where
import Data.Text hiding (filter, map)
import qualified Data.Map.Strict as Map

import Statement

data Location = Location { actor:: Text,
                            target:: Text
                            } deriving Show

locationTokens = [ pack "moved", pack "journeyed", pack "went"]

makeLocation :: Statement -> Location
makeLocation  statement = 
    Location (Statement.actor statement) (Statement.target statement)

parseLocations :: [Statement] -> [Location]
parseLocations statemens =
       map makeLocation $ filter statementFilter statemens 

statementFilter statement =
    elem (Statement.action statement) locationTokens