{-# LANGUAGE DataKinds, GADTs, TypeFamilies, ExistentialQuantification, TypeOperators #-}
------------------------------------------------------------------------
--
-- A solver is a function from a list of pieces and a space to be filled
-- and returns all possible permutataions of those peices in that space.
-- 
-- definition of the peices and space share di-
------------------------------------------------------------------------

module MDSolver () where

import Data.List.Split
import Data.List
import Dependent
import Control.Applicative

main = do 
    puzzle <- readPuzzle parser3D "board.txt" pieces
    maybe (putStrLn "Error Reading puzzle files")
          (\(_,pieces) -> mapM_ (putStr . (pretty parser3D)) pieces )
          {-((mapM_ printShape) . solve rules)-}
          puzzle

parser3D = "\n\n" :- ("\n" :- Nil)
pieces = [ "piece" ++ (show x) ++ ".txt" | x <- [0..5]]

type Shape n = Space n Char  
type Board n = Shape n
type Piece n = Shape n
type Puzzle n = (Board n, [Piece n])
type Parser n = Vector n String

pretty :: (Show a) => Parser n -> Space (S n) a -> String
pretty Nil       (Layer as) = concatMap show as 
pretty (p :- ps) (Layer as) = concat $ (intersperse p) $ map (pretty ps) as

readPuzzle :: Parser n -> FilePath -> [FilePath] -> IO (Maybe (Puzzle (S n)))
readPuzzle p b ps = do
    bd  <- readFile b
    pcs <- mapM readFile ps
    return $ do
        board  <- parse p bd
        pieces <- mapM (parse p) ps
        return (board, pieces)
        

parse :: Parser n -> String -> Maybe (Shape (S n))
parse Nil       s = Just . Layer $ map Atom s
parse (p :- ps) s = Just . Layer <$> map (parse ps) (splitOn p s)

printSpace :: Space n Char -> IO ()
printSpace = undefined

solve :: (Char -> Char -> Maybe Char) -> Puzzle n -> [Board n]
solve = undefined

rules :: Char -> Char -> Maybe Char
rules '_'  b  = Just b
rules  a  '_' = Just a
rules  _   _  = Nothing

knead :: (a -> a -> Maybe a) -> Space n a -> Space n a -> Maybe (Space n a)
knead = undefined
{-knead f (Atom a) (Atom b) = f a b-}
{-knead _ _ zs     []     = Just zs-}
{-knead _ _ []     [x]    = Nothing-}
{-knead f 0 (a:as) (b:bs) = (:) <$> f a b  <*> knead f 0     as bs-}
{-knead f x (a:as) bs     = (:) <$> Just a <*> knead f (x-1) as bs-}
