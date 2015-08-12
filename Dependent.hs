{-# LANGUAGE DataKinds, GADTs, TypeFamilies, ExistentialQuantification, TypeOperators #-}

module Dependent 
   ( Nat(..)
   , Vector(..)
   , Space(..)
   , fromList
   , depth
   )
where

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
    Layer :: Vector b (Space n a) -> Space (S n) a

instance Functor (Space n) where
    fmap f (Atom a)   = Atom (f a)
    fmap f (Layer ss) = Layer $ map (fmap f) ss

fromList :: [a] -> Vector n a
fromList []     = Nil
formList (x:xs) = x :- xs

depth :: Space a b -> Int
depth (Atom _) = 0
depth (Layer (a:_)) = 1 + depth a
