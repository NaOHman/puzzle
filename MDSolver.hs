{-# LANGUAGE DataKinds, GADTs, TypeFamilies, ExistentialQuantification, TypeOperators #-}
------------------------------------------------------------------------
--
-- A solver is a function from a list of pieces and a space to be filled
-- and returns all possible permutataions of those peices in that space.
-- 
-- definition of the peices and space share di-
------------------------------------------------------------------------

module MDSolver (main) where

import Data.List hiding (map)
import qualified Data.Map as M
import Control.Monad

main = do 
    puzzle <- readPuzzle parser3D "board.txt" pieces
    maybe (putStrLn "Error Reading puzzle files")
          (\(_,pieces) -> mapM_ (putStr . (pretty parser3D)) pieces )
          {-((mapM_ printSpace) . solve rules)-}
          puzzle

parser3D = ["\n\n", "\n"]
pieces = [ "piece" ++ (show x) ++ ".txt" | x <- [0..5]]

type Board a = Space a
type Piece a = Space a
type Puzzle a = (Board a, [Piece a])
type Parser = [String]
type Coordinate = [Int]

instance (Show a) => Show (Space a) where
    show (Atom a)   = show a
    show (Layer as) = concatMap show as

data Space a where
    Atom  :: a -> Space a
    Layer :: M.Map Int (Space a) -> Space a

instance Functor Space where
    fmap f (Atom a)   = Atom (f a)
    fmap f (Layer m) = Layer $ fmap (fmap f) m

pretty :: (Show a) => [String] -> Space a -> String
pretty []     (Layer as) = concatMap show as 
pretty (p:ps) (Layer as) = concat $ intersperse p $ children as
    where children = map (pretty ps) . M.elems

readPuzzle :: Parser -> FilePath -> [FilePath] -> IO (Maybe (Puzzle Char))
readPuzzle p b ps = do
    bd  <- readFile b
    pcs <- mapM readFile ps
    return $ do
        board  <- parse p bd
        pieces <- mapM (parse p) ps
        return (board, pieces)

parse :: Parser -> String -> Maybe (Space a)
parse = undefined
{-parse Nil       = Layer . (map Atom)-}
{-parse (p :- ps) = Layer . (map (parse ps)) . (splitOn p)-}

printSpace :: Space Char -> IO ()
printSpace = undefined

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

-- attempt to add a layer to the nth dimension of the Space
layer :: Int -> Space a -> Space a -> Maybe (Space a)
layer = undefined

getAtom :: Coordinate -> Space a -> Maybe a
getAtom c s = getChunk c c s >>= get

-- pulls a chunk out of the space bound by the two coordinates
-- Coordinates must be the same length, 0 width dimensions are omitted
getChunk :: Coordinate -> Coordinate -> Space a -> Maybe (Space a)
getChunk = undefined

getAtomWith :: (a -> Maybe a) -> Coordinate -> Space a -> Maybe a
getAtomWith f (x:xs) (Layer m) = M.lookup x m >>= getAtomWith f xs
getAtomWith f []     (Atom a)  = f a
getAtomWith _ _      _         = Nothing
    
setAtomWith :: (a -> Maybe a) -> Coordinate -> Space a -> Maybe (Space a)
setAtomWith f (x:xs) (Layer m) = 
    M.lookup x m >>= setAtomWith f xs >>= \s -> Just $ Layer $ M.insert x s m
setAtomWith f []     (Atom a)  = fmap Atom $ f a
setAtomWith _ _      _         = Nothing

atom' a@(Atom _) = Just a
atom' _          = Nothing

get :: Space a -> Maybe a
get (Atom a) = Just a
get _        = Nothing

layer' l@(Layer _) = Just l
atom _             = Nothing
