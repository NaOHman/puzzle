module Shape 
    ( Shape(..)
    , Coordinate
    , adjust
    , bounds
    , empty
    , fromList
    , insert
    , lookup
    , pretty
    , promote
    , rotate
    , singleton
    , subspace
    , transform
    , union
    , unionWith
    ) where

import qualified Data.IntMap.Strict as M
import qualified Data.List as L 
import Prelude hiding (lookup, span)
import Control.Monad
import Data.Maybe

-- Type definitions

type Coordinate = [Int]

data Shape a = Atom a | Layer (M.IntMap (Shape a))

instance (Show a) => Show (Shape a) where
    show (Atom a)      = show a
    show (Layer m) = concatMap show m

instance Functor Shape where
    fmap f (Atom a)  = Atom $ f a
    fmap f (Layer m) = Layer $ fmap (fmap f) m

-- Gets the value at the given coordinate
lookup :: Coordinate -> Shape a -> Maybe a
lookup []     (Atom a)  = Just a
lookup (c:cs) (Layer l) = M.lookup c l >>= lookup cs
lookup _      _         = Nothing

-- modifies the falue at the give coordinate if it exists
adjust :: (a -> a) -> Coordinate -> Shape a -> Shape a
adjust f []     (Atom a) = Atom $ f a
adjust _ []     s        = s
adjust f (c:cs) s        = withLayer s $ M.adjust (adjust f cs) c

traverse :: Applicative t => (Coordinate -> a -> t b) -> Shape a -> t (Shape b)
traverse f = traverse' f []
traverse' f cs (Atom a)  = Atom <$> f (reverse cs) a
traverse' f cs (Layer l) = Layer <$> M.traverseWithKey (traverse' f . (:cs)) l
 
transform :: (a -> M.IntMap (Shape b) -> M.IntMap (Shape b)) -> [a] -> Shape b -> Shape b
transform _ [] s = s
transform f (c:cs) l = withLayer l $ f c . fmap (transform f cs)

rotate :: [Bool] -> Shape a -> Shape a
rotate = transform $ \b m -> if b then flipKeys m else m

flipKeys :: M.IntMap a -> M.IntMap a
flipKeys m = M.mapKeys (range -) m
    where range = fst (M.findMax m) + fst (M.findMin m)

unionWith :: (Shape a -> Shape a -> Shape a) -> Shape a -> Shape a -> Shape a
unionWith f (Layer a) (Layer b) = Layer $ M.unionWith f a b
unionWith f a         b         = f a b

{-unionAcc :: Traversable t-}

union :: Shape a -> Shape a ->  Shape a
union = unionWith const

{-unionsWith = (Shape a -> Shape a -> Shape a) -> [Shape a] -> Shape a-}
{-unionsWith f = foldl' (unionWith f) empty -}

insert :: Coordinate -> a -> Shape a -> Shape a 
insert c a = union (singleton c a)

empty = Layer M.empty

singleton :: Coordinate -> a -> Shape a
singleton []     = Atom
singleton (c:cs) = Layer . M.singleton c . singleton cs

promote :: Shape a -> Shape a
promote a@(Atom _) = Layer $ M.singleton 0 a
promote l          = l

withLayer :: Shape a -> (M.IntMap (Shape a) -> M.IntMap (Shape a)) -> Shape a
withLayer (Layer m) f = Layer $ f m
withLayer s _         = s

shift :: Coordinate -> Shape a -> Shape a
shift = transform (\a -> M.mapKeysMonotonic (a+))

subspace :: M.Key -> Shape a -> Maybe (Shape a)
subspace c (Layer m) = M.lookup c m
subspace _ _         = Nothing

isAtom (Atom _) = True
isAtom _        = False

isLayer = not . isAtom

get :: Shape a -> Maybe a
get (Atom a) = Just a
get _        = Nothing

fromList :: [a] -> Shape a
fromList = Layer . M.fromList . zipWith (\k v -> (k, Atom v)) [0..]

span :: Ord a => [a] -> (a,a)
span (x:xs) = L.foldl' (\a b -> wider a (b,b)) (x,x) xs

reduce :: ([a] -> a) -> [[a]] -> [a] 
reduce _ ([]:_) = []
reduce f xs     = f (map head xs) : reduce f (map tail xs)
    
wider :: Ord a => (a,a) -> (a,a) -> (a,a)
wider (a,b) (c,d) = (min a c, max b d)

widest :: Ord a => [(a,a)] -> (a,a)
widest (x:xs) = L.foldl' wider x xs

bounds :: Shape a -> [(M.Key, M.Key)]
bounds (Atom a)  = []
bounds (Layer l) = let b  = span $ M.keys l
                       bs = reduce widest $ M.foldr' ((:) . bounds) [] l
                   in b:bs
-- Printing
pretty :: [String] -> Shape Char -> String
pretty ds s = pretty' (map snd $ bounds s) ds s

pretty' :: [Int] -> [String] -> Shape Char -> String
pretty' _  _  (Atom a)   = a:""
pretty' (l:ls) (d:ds) (Layer m) = snd $ M.foldlWithKey f (0,"") m
    where f (len,s) k v = (k, prefix (k - len - 1) ++ pretty' ls ds v ++ s)
          prefix 0 = " "
          prefix n = p (n:ls) (d:ds)
          p []      []    = " "
          p (l:ls) (d:ds) = L.intercalate d $ replicate l $ p ls ds

    {-= intercalate d $ zipWith mult ls ds-}
          {-mult l:_ []    = replicate l ' '-}
          {-mult l:ls d:ds = intercalate d $ replicate l $ mult ls ds-}
          {-prefix -}
          {-ls = snd b : -}
          
    {-L.intercalate p $ map (pretty e ps) (M.elems as)-}


reEscape :: String -> String
reEscape ('\\':'n':ss) = '\n' : reEscape ss
reEscape ('\'':ss)     = reEscape ss
reEscape (s:ss)        = s : reEscape ss
reEscape _             = ""
