module Parse 
    ( parseShape
    ) where

import Text.ParserCombinators.Parsec
import Space
import Control.Monad

parseShape :: [String] -> GenParser Char st (Space Char)
parseShape (x:xs) = liftM fromList (sepBy (parseShape xs) (string x))
parseShape []     = do
    cs <- many anyChar
    return $ fromList <$> mapM (Atom . p) cs
    where p '-' = Atom Nothing
          p  c  = Atom . Just c 
