module Statement where

import Data.Text
import Data.Char

data Statement = Statement { actor:: Text,
                             action:: Text,
                             target:: Text
                            } deriving Show
buildStatement :: Text -> Statement
buildStatement txt = 
    let actorT = breakOn (pack " ")(Data.Text.toLower txt)
        statemetT =  breakOn (pack "the") (snd actorT)
    in Statement (cleanText (fst actorT)) (cleanText(fst statemetT)) (cleanText (snd statemetT))

cleanText textPart =
   (Data.Text.filter (\t ->  not (isSpace t)) 
   . replace (pack "to") (pack "") 
   . replace (pack "the") (pack ""))(textPart)