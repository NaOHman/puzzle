{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeOperators #-}
------------------------------------------------------------------------
--
-- A solver is a function from a list of pieces and a space to be filled
-- and returns all possible permutataions of those peices in that space.
-- 
-- definition of the peices and space share di-
------------------------------------------------------------------------
type Board n = Space Char n
type Piece n = Space Char n
type Puzzle n = (Board n, [Piece n])
type ThreeDSpace = Space Char ThreeD

data Nat = Z | S Nat

type ThreeD = S (S (S Z))

infixl 6 :+

type family (n :: Nat) :+ (m :: Nat) :: Nat
type instance Z        :+ m = m
type instance (S n)    :+ m = S (n :+ m)

data Vector a n where
    Nil  :: Vector a Z
    (:-) :: a -> Vector a n -> Vector a (S n)

data Space a n where
    Atom  :: a -> Space a Z
    Layer :: [Space a n] -> Space a (S n)

main = do 
    puzzle <- readPuzzle "board.txt" ["pieces.txt"]
    maybe (putStrLn "Error Reading puzzle files")
          ((mapM_ printSpace) . solve rules)
          puzzle

readPuzzle :: FilePath -> [FilePath] -> IO (Maybe (Puzzle n))
readPuzzle = undefined

printSpace :: Space Char n -> IO ()
printSpace = undefined

solve :: (Char -> Char -> Maybe Char) -> Puzzle n -> [Board n]
solve = undefined

rules :: Char -> Char -> Maybe Char
rules '_'  b  = Just b
rules  a  '_' = Just a
rules  _   _  = Nothing


knead :: (a -> a -> Maybe a) -> Space a b -> Space a b -> Maybe (Space a b)
knead = undefined
{-knead f (Atom a) (Atom b) = f a b-}
{-knead _ _ zs     []     = Just zs-}
{-knead _ _ []     [x]    = Nothing-}
{-knead f 0 (a:as) (b:bs) = (:) <$> f a b  <*> knead f 0     as bs-}
{-knead f x (a:as) bs     = (:) <$> Just a <*> knead f (x-1) as bs-}

depth :: Space a b -> Int
depth (Atom _) = 0
depth (Layer (a:_)) = 1 + depth a
