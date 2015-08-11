import Data.List 
import Data.Maybe 
import Control.Applicative

type Puzzle = [[Char]]
type Piece = [[Char]]
type Position = (Int,Int)

main = printMatrices $ cleanSolution $ solutions myPieces emptyPuzzle 

cleanSolution = nub . map (minimum . spins)

genSolutions :: [Piece] -> Puzzle -> [Puzzle]
genSolutions (pc:pcs) pz = concatMap (solutions pcs) nextPos 
    where nextPos = concatMap putFirst $ spins pc
          putFirst c = mapMaybe (sits pz c) firstPlaces

solutions :: [Piece] -> Puzzle -> [Puzzle]
solutions []  pz      = [pz]
solutions (pc:pcs) pz = concatMap (solutions pcs) nextPos
    where nextPos = putSpins pc pz

putSpins :: Puzzle -> Piece -> [Puzzle]
putSpins pz = concatMap (putPiece pz) . spins

putPiece :: Piece -> Puzzle -> [Puzzle]
putPiece pc pz = mapMaybe (sits pz pc) (places pz pc) 

spins :: Piece -> [Piece]
spins m = nub $ spin m >>= mirror >>= turn
    where spin   p = [p, transpose p]
          turn   p = [p, reverse p]
          mirror p = [p, map reverse p]

places :: Puzzle -> Piece -> [Position]
places (d:ds) (s:ss) = [(x,y) | x <- [0..r], y <- [0..c]]
    where r = length (d:ds) - length (s:ss)
          c = length d - length s

firstPlaces :: [Position]
firstPlaces = [(x,y) | x <- [0..3], y <- [0..x]]

sits :: Puzzle -> Piece -> Position -> Maybe Puzzle
sits pz [] _ = Just pz
sits (z:zs) (c:cs) (0,n) = (:) <$> sit z c n <*> sits zs cs (0,n)
sits (z:zs) pc     (r,n) = (:) <$> Just z    <*> sits zs pc (r-1,n)

sit :: [Char] -> [Char] -> Int -> Maybe [Char]
sit d      []     _ = Just d 
sit (d:ds) (s:ss) 0 
    | d == '_'  = (:) <$> Just s <*> sit ds ss 0
    | s == '_'  = (:) <$> Just d <*> sit ds ss 0
    | otherwise = Nothing
sit (d:ds)  s     n = (:) <$> Just d <*> sit ds s (n-1)

myPieces :: [Piece]
myPieces = [orange, cyan, yellow, purple, brown, green, red, blue]

emptyPuzzle :: Puzzle
emptyPuzzle = ["________",
               "________",
               "________",
               "________",
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

printMatrix m = putStr (unlines m)
printMatrices m = putStr $ unlines $ map unlines m
