{-# LANGUAGE DataKinds, GADTs, TypeFamilies, ExistentialQuantification, TypeOperators #-}
module Space 
    ( Space(..)
    , Coordinate
    , fromList
    , get
    , getAtom
    , modifyAtom
    , pretty
    , setAtom
    ) where

import Data.List hiding (map)
import qualified Data.Map as M
import Control.Monad

type Coordinate = [Int]

data Space a where
    Atom  :: a -> Space a
    Layer :: M.Map Int (Space a) -> Space a

instance (Show a) => Show (Space a) where
    show (Atom a)   = show a
    show (Layer as) = concatMap show as

instance Functor Space where
    fmap f (Atom a)   = Atom (f a)
    fmap f (Layer m) = Layer $ fmap (fmap f) m

pretty :: (Show a) => [String] -> Space a -> String
pretty []     (Layer as) = reEscape $ concatMap show as 
pretty (p:ps) (Layer as) = intercalate p $ map (pretty ps) (M.elems as)

reEscape :: String -> String
reEscape ('\\':'n':ss) = '\n' : reEscape ss
reEscape ('\'':ss)     = reEscape ss
reEscape (s:ss)        = s : reEscape ss
reEscape _             = ""

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

modifyAtom :: (a -> Maybe a) -> Coordinate -> Space a -> Maybe (Space a)
modifyAtom f (x:xs) (Layer m) = 
    M.lookup x m >>= modifyAtom f xs >>= \s -> Just $ Layer $ M.insert x s m
modifyAtom f []     (Atom a)  = Atom <$> f a
modifyAtom _ _      _         = Nothing

setAtom :: a -> Coordinate -> Space a -> Maybe (Space a)
setAtom = modifyAtom . const . Just

atom' a@(Atom _) = Just a
atom' _          = Nothing

layer' l@(Layer _) = Just l
layer' _             = Nothing

get :: Space a -> Maybe a
get (Atom a) = Just a
get _        = Nothing

fromList :: [Space a] -> Space a
fromList ss = Layer $ snd $ foldl p (0, M.empty) ss
    where p (x, m) s = (x+1, M.insert x s m)
