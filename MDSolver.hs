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
import qualified Data.IntMap as M
import Space
import Control.Monad
import Text.ParserCombinators.Parsec

type Board a  = Space a
type Piece a  = Space a
type Puzzle a = (Board a, [Piece a])

main = do 
    puzzle <- readPuzzle (parseShape delims) "board.txt" pieces
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
rules c   '_' = Just c
rules '_' c   = Just c
rules _   _   = Nothing


-- Attempts to fit the first space into the second space
-- if the second space does not have an entry corresponding to the
-- second space return nothing.
fit :: (a -> a -> Maybe a) -> Space a -> Space a -> Maybe (Space a)
fit f (Atom a)  (Atom b)  = Atom <$> f a b
fit f (Layer a) (Layer b) = Layer <$> M.fromList <$> mapM fit' (M.assocs a) 
    where fit' (k,v) = M.lookup k b >>= fit f v >>= \s -> Just (k, s)
fit _ _ _                 = Nothing


