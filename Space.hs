{-# LANGUAGE DataKinds, GADTs, TypeFamilies, ExistentialQuantification, TypeOperators #-}
module Space 
    ( Space(..)
    , Coordinate
    , fromList
    , get
    {-, modifyAtom-}
    , pretty
    {-, setAtom-}
    ) where

import Data.List hiding (map, insert, lookup)
import Prelude hiding (lookup)
import qualified Data.IntMap.Strict as M
import Control.Monad
import Data.Maybe

-- Type definitions

type Coordinate = [Int]

data Space a = Atom a | Layer (M.IntMap (Space a))

instance (Show a) => Show (Space a) where
    show (Atom a)      = show a
    show (Layer m) = concatMap show m

instance Functor Space where
    fmap f (Atom a)  = Atom $ f a
    fmap f (Layer m) = Layer $ fmap (fmap f) m

{-normal :: Space a -> Bool-}
{-normal (Node m) = all atom' ss || (all layer' ss && all normal ss)-}
    {-where ss = M.elems m-}
{-normal (Atom  a) = True-}

-- knead == unionWith
{-knead :: (a -> a -> Maybe a) -> Space a -> Space a -> Maybe (Space a)-}
{-knead = undefined-}
{-knead f (Atom a) (Atom b) = f a b-}
{-knead _ _ zs     []     = Just zs-}
{-knead _ _ []     [x]    = Nothing-}
{-knead f 0 (a:as) (b:bs) = (:) <$> f a b  <*> knead f 0     as bs-}
{-knead f x (a:as) bs     = (:) <$> Just a <*> knead f (x-1) as bs-}

-- pulls a chunk out of the space bound by the two coordinates
-- Coordinates must be the same length, 0 width dimensions are omitted
{-getChunk :: Coordinate -> Coordinate -> Space a -> Maybe (Space a)-}
{-getChunk = undefined-}


lookup :: Coordinate -> Space a -> Maybe a
lookup []     (Atom a)  = Just a
lookup (c:cs) (Layer l) = M.lookup c l >>= lookup cs
lookup _      _         = Nothing

adjust :: (a -> a) -> Coordinate -> Space a -> Space a
adjust f []     (Atom a) = Atom $ f a
adjust f (c:cs) s        = withLayer s $ M.adjust (adjust f cs) c

insert :: Coordinate -> a -> Space a -> Space a
insert []     a _          = Atom a
insert cs     a s@(Atom _) = insert cs a $ promote s
insert (c:cs) a l = withLayer l $ M.insert c sublayer
    where sublayer = insert cs a $ fromMaybe emptyLayer (subspace c l)

promote :: Space a -> Space a
promote a@(Atom _) = Layer $ M.singleton 0 a
promote l          = l

withLayer :: Space a -> (M.IntMap (Space a) -> M.IntMap (Space a)) -> Space a
withLayer (Layer m) f = Layer $ f m
withLayer s _         = s

isAtom (Atom _) = True
isAtom _        = False

isLayer = not . isAtom

get :: Space a -> Maybe a
get (Atom a) = Just a
get _        = Nothing

subspace :: M.Key -> Space a -> Maybe (Space a)
subspace c (Layer m) = M.lookup c m
subspace _ _         = Nothing

emptyLayer = Layer M.empty

fromList :: [a] -> Space a
fromList = Layer . M.fromList . zipWith (\k v -> (k, Atom v)) [0..]

-- Printing
pretty :: (Show a) => [String] -> Space a -> String
pretty _      (Atom a)   = show a
pretty []     (Layer as) = reEscape $ concatMap show as 
pretty (p:ps) (Layer as) = intercalate p $ map (pretty ps) (M.elems as)

reEscape :: String -> String
reEscape ('\\':'n':ss) = '\n' : reEscape ss
reEscape ('\'':ss)     = reEscape ss
reEscape (s:ss)        = s : reEscape ss
reEscape _             = ""
