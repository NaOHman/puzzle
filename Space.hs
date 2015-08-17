{-# LANGUAGE DataKinds, GADTs, TypeFamilies, ExistentialQuantification, TypeOperators #-}
module Space 
    ( Space(..)
    , Coordinate
    , fromList
    , get
    , getAtom
    {-, modifyAtom-}
    , pretty
    {-, setAtom-}
    ) where

import Data.List hiding (map, insert)
import qualified Data.IntMap as M
import Control.Monad
import Data.Maybe

-- Type definitions

type Coordinate = [Int]

data Space a where
    Atom  :: a -> Space a
    Layer :: M.IntMap (Space a) -> Space a

instance (Show a) => Show (Space a) where
    show (Atom a)   = show a
    show (Layer as) = concatMap show as

instance Functor Space where
    fmap f (Atom a)  = Atom (f a)
    fmap f (Layer m) = Layer $ fmap (fmap f) m

instance Applicative Space where
    pure  = return
    (<*>) = ap

instance Monad Space where
    return = Atom
    (Atom a)  >>= f = f a
    (Layer m) >>= f = Layer $ fmap (>>= f) m

normal :: Space a -> Bool
normal (Layer m) = all atom' ss || (all layer' ss && all normal ss)
    where ss = M.elems m
normal (Atom  a) = True

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

adjust :: (Space a -> Space a) -> Coordinate -> Space a -> Maybe (Space a)
adjust f (c:cs) (Layer m) = Layer <$> do
    s  <- M.lookup c m
    s' <- adjust f cs s
    return $ M.insert c s' m
adjust f [] s = Just $ f s
adjust _ _  _ = Nothing

insert :: Coordinate -> a -> Space a -> Space a
insert []     a _ = Atom a
insert (c:cs) a (Layer m) = Layer $ M.insert c (insert cs a s) m
    where s = fromMaybe emptyLayer (M.lookup c m)
insert cs     a s  = insert cs a $ promote s

promote :: Space a -> Space a
promote s = Layer $ M.singleton 0 s

withLayer :: (M.IntMap (Space a) -> M.IntMap (Space a)) -> Space a -> Maybe (Space a)
withLayer f (Layer m) = Just . Layer $ f m
withLayer _ _         = Nothing

withNth :: (Space a -> Space a) -> M.Key -> Space a -> Maybe (Space a)
withNth f k (Layer m) = f <$> M.lookup k m
withNth f 0 a         = Just $ f a
withNth _ _ _         = Nothing

{-modifyAtom :: (a -> a) -> Coordinate -> Space a -> Maybe (Space a)-}
{-modifyAtom f (x:xs) (Layer m) = M.adjust (modifyAtom f xs) x m-}
{-modifyAtom f []     (Atom a)  = Atom  $ f a-}
{-modifyAtom _ _      s         = s-}

{-setAtom :: a -> Coordinate -> Space a -> Maybe (Space a)-}
{-setAtom = modifyAtom . const . Just-}

atom' a@(Atom _) = True
atom' _          = False

layer' l@(Layer _) = True
layer' _           = False

get :: Space a -> Maybe a
get (Atom a) = Just a
get _        = Nothing

emptyLayer = Layer M.empty

fromList :: [a] -> Maybe (Space a)
fromList []  = Nothing
fromList [a] = Just $ Atom a
fromList l   = Just $ foldl f (Layer M.empty) (zip [0..] l)
    where f s (k, v) = insert [k] v s

-- Printing
pretty :: (Show a) => [String] -> Space a -> String
pretty []     (Layer as) = reEscape $ concatMap show as 
pretty (p:ps) (Layer as) = intercalate p $ map (pretty ps) (M.elems as)

reEscape :: String -> String
reEscape ('\\':'n':ss) = '\n' : reEscape ss
reEscape ('\'':ss)     = reEscape ss
reEscape (s:ss)        = s : reEscape ss
reEscape _             = ""
