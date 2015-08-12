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


data Nat = Z | S Nat

infixl 6 :+

type family (n :: Nat) :+ (m :: Nat) :: Nat
type instance Z        :+ m = m
type instance (S n)    :+ m = S (n :+ m)

data Vector n a where
    Nil  :: Vector Z a
    (:-) :: a -> Vector n a -> Vector (S n) a

instance Foldable (Vector n) where
    foldMap f Nil    = mempty
    foldMap f (a:-b) = (f a) `mappend` foldMap f b

instance (Show a) => Show (Space n a) where
    show (Atom a)   = show a
    show (Layer as) = concatMap show as

data Space n a where
    Atom  :: a -> Space Z a
    Layer :: [Space n a] -> Space (S n) a

instance Functor (Space n) where
    fmap f (Atom a)   = Atom (f a)
    fmap f (Layer ss) = Layer $ map (fmap f) ss

pretty :: (Show a) => Parser n -> Space (S n) a -> String
pretty Nil       (Layer as) = concatMap show as 
pretty (p :- ps) (Layer as) = concat $ (intersperse p) $ map (pretty ps) as

readPuzzle :: Parser n -> FilePath -> [FilePath] -> IO (Maybe (Puzzle (S n)))
readPuzzle p b ps = do
    bd  <- readFile b
    pcs <- mapM readFile ps
    do
        board  <- parse p bd
        pieces <- mapM (parse p) ps
        (board, pieces)
        

parse :: Parser n -> String -> Maybe (Shape (S n))
parse Nil       = Layer . (map Atom)
parse (p :- ps) = Layer . (map (parse ps)) . (splitOn p)

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

depth :: Space a b -> Int
depth (Atom _) = 0
depth (Layer (a:_)) = 1 + depth a
