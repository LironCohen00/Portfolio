{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Deque (Deque, empty, pushr, pushl, popr, popl) where

import Control.Applicative (liftA2)
import Data.Foldable

empty :: Deque a
empty = Deque [] []

data Deque a = Deque [a] [a]

instance Eq a => Eq (Deque a) where
  Deque l r == Deque l' r' = l ++ reverse r == l' ++ reverse r'
instance Show a => Show (Deque a) where
  show (Deque l r) = show (l ++ reverse r)

pushl :: a -> Deque a -> Deque a
pushl x (Deque l r) = Deque (x : l) r

pushr :: a -> Deque a -> Deque a
pushr x (Deque l r) = Deque l (x : r)

popl :: Deque a -> Maybe (a, Deque a)
popl = \case
  Deque [] [] -> Nothing
  Deque (l : ls) r -> Just (l, Deque ls r)
  Deque [] r -> popl $ Deque (reverse r) []

popr :: Deque a -> Maybe (a, Deque a)
popr = \case
  Deque [] [] -> Nothing
  Deque l (r : rs) -> Just (r, Deque l rs)
  Deque l [] -> popr $ Deque [] (reverse l)

instance Semigroup (Deque a) where
  (<>) :: Deque a -> Deque a -> Deque a
  (Deque l r) <> (Deque l' r') = Deque (l ++ reverse r) (r' ++ reverse l')
instance Monoid (Deque a) where
  mempty :: Deque a
  mempty = Deque [] []
  mappend = (<>)
instance Foldable Deque where
  foldMap f (Deque l r) = foldMap f (l ++ reverse r)
instance Functor Deque where
  fmap mapping (Deque l r) = Deque (map mapping l) (map mapping r) 
instance Applicative Deque where
  pure a = Deque [a] []
  liftA2 _ (Deque [] []) _ = Deque [] []
  liftA2 f (Deque (l : ls) r) y = fmap (f l) y <> liftA2 f (Deque ls r) y
  liftA2 f (Deque [] r) y = liftA2 f (Deque (reverse r) []) y
instance Monad Deque where
  (Deque [] []) >>= _ = Deque [] []
  (Deque [] r) >>= f = (>>=) (Deque (reverse r) []) f
  (Deque (l : ls) r) >>= f = f l <> (>>=) (Deque ls r) f
