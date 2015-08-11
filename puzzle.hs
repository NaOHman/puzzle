import Data.List 
import Data.Maybe
import Control.Applicative

type Row = [Char]
type Puzzle = [Row]
type Piece =  [Row]
type Position = (Int,Int)
type PieceLocation = ([Position], Piece)

main = printMatrices $ solve myPieces emptyPuzzle

solve :: [Piece] -> Puzzle -> [Puzzle]
solve cs z = unique $ solutions (makeLocations z cs) z 
    where unique = nub . map (minimum . spins)

solutions :: [[PieceLocation]] -> Puzzle -> [Puzzle]
solutions [] z     = [z]
solutions (l:ls) z = concatMap (solutions ls) $ validPlacements z
    where validPlacements pz = concatMap placeInLocations l
          placeInLocations (ps, c) = mapMaybe (place z c) ps

place :: Puzzle -> Piece -> Position -> Maybe Puzzle
place z c (x,y) = knead (knead placeOne x) y z c
    where placeOne '_' b = Just b
          placeOne  a '_' = Just a
          placeOne  _  _ = Nothing

knead :: (a -> b -> Maybe a) -> Int -> [a] -> [b] -> Maybe [a]
knead _ _ zs     []     = Just zs
knead _ _ []     [x]    = Nothing
knead f 0 (a:as) (b:bs) = (:) <$> f a b  <*> knead f 0     as bs
knead f x (a:as) bs     = (:) <$> Just a <*> knead f (x-1) as bs


makeLocations :: Puzzle -> [Piece] -> [[PieceLocation]]
makeLocations z (c:cs) = firstLocations c : locations cs
    where locations       = map (map (positions z) . spins)
          firstLocations  = map ((,) [(x,y) | x <- [0..l], y <-[0..x]]) . spins
          l               = ceiling (realToFrac (length z) / 2) - 1

spins :: Piece -> [Piece]
spins c = nub $ transpose' c >>= reverse' >>= rotate'
    where transpose' a = [a, transpose a]
          reverse'   a = [a, reverse a]
          rotate'    a = [a, map reverse a]

positions :: Puzzle -> Piece -> PieceLocation
positions z@(d:ds) p@(s:ss) = ([(x,y) | x <- [0..r], y <- [0..c]], p)
    where r = length z - length p 
          c = length d - length s

-------------------------------------------------------------------------------
--                             Puzzle Definition                             --
-------------------------------------------------------------------------------

myPieces :: [Piece]
myPieces = [orange, cyan, yellow, purple, brown, green, red, blue]
myStrings = ["orange", "cyan", "yellow", "purple", "brown", "green", "red", "blue"]

emptyPuzzle :: Puzzle
emptyPuzzle = ["________",
               "________",
               "________",
               "________",
               "________",
               "________",
               "________",
               "________"]

testPieces = [orange, cyan, yellow, purple, brown, green, red]
testStrings = ["orange", "cyan", "yellow", "purple", "brown", "green", "red"]
testPuzzle  = ["bbb_____",
               "bbb_____",
               "__b_____",
               "__b_____",
               "________",
               "________",
               "________",
               "________"]


green :: Piece
green = ["g__g",
         "g__g",
         "gggg"]

cyan :: Piece
cyan = ["_cc_",
        "_cc_",
        "cccc"]

yellow :: Piece
yellow = ["yy__",
          "yyyy",
          "yy__"]
        
brown :: Piece
brown = ["__nn",
         "nnnn",
         "nn__"]

red :: Piece
red = ["__rrr",
       "rrrrr"]

purple :: Piece
purple = ["pp_",
          "ppp",
          "ppp"]

blue :: Piece
blue = ["bb__",
        "bb__",
        "bbbb"]

orange :: Piece
orange = ["aaaa",
          "aaaa"]

printMatrix :: [Row] -> IO ()
printMatrix m = putStr (unlines m)

printMatrices :: [[Row]] -> IO ()
printMatrices m = putStr $ unlines $ map unlines m
