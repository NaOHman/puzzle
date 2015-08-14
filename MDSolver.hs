{-# LANGUAGE DataKinds, GADTs, TypeFamilies, ExistentialQuantification, TypeOperators #-}
------------------------------------------------------------------------
--
-- A solver is a function from a list of pieces and a space to be filled
-- and returns all possible permutataions of those peices in that space.
-- 
-- definition of the peices and space share di-
------------------------------------------------------------------------

module Main (main) where

import Data.List hiding (map)
import qualified Data.Map as M
import Space
import Control.Monad
import Text.ParserCombinators.Parsec

type Board a = Space a
type Piece a = Space a
type Puzzle a = (Board a, [Piece a])

main = do 
    puzzle <- readPuzzle (parseSpace delims) "board.txt" pieces
    either print
          (\(_,pieces) -> mapM_ (putStrLn . pretty delims) pieces )
          puzzle
    where
        delims = ["\n\n", "\n"]
        pieces = [ "piece" ++ show x ++ ".txt" | x <- [0..5]]

          {-((mapM_ printSpace) . solve rules)-}

readPuzzle :: GenParser Char () (Space a) -> FilePath -> [FilePath] -> IO (Either ParseError (Puzzle a))
readPuzzle p b ps = do
    bd  <- readFile b
    pcs <- mapM readFile ps
    return $ do
        board  <- parse p b bd
        pieces <- zipWithM (parse p) ps pcs
        return (board, pieces)

solve :: (Char -> Char -> Maybe Char) -> Puzzle n -> [Board n]
solve = undefined

rules :: Char -> Char -> Maybe Char
rules '_'  b  = Just b
rules  a  '_' = Just a
rules  _   _  = Nothing

knead :: (a -> a -> Maybe a) -> Space a -> Space a -> Maybe (Space a)
knead = undefined
{-knead f (Atom a) (Atom b) = f a b-}
{-knead _ _ zs     []     = Just zs-}
{-knead _ _ []     [x]    = Nothing-}
{-knead f 0 (a:as) (b:bs) = (:) <$> f a b  <*> knead f 0     as bs-}
{-knead f x (a:as) bs     = (:) <$> Just a <*> knead f (x-1) as bs-}

parseSpace :: [String] -> GenParser Char st (Space Char)
parseSpace (x:xs) = liftM fromList (sepBy (parseSpace xs) (string x))
parseSpace []     = do
    cs <- many anyChar
    return $ fromList $ map Atom cs
