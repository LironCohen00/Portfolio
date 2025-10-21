{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module EqSet (
  EqSet,
  empty,
  EqSet.insert, -- To avoid name clash with Data.List.insert
  member,
  remove,
  elems,
) where

import Data.Either
import Data.List
import Data.Maybe

newtype EqSet a = EqSet [a]

empty :: EqSet a
empty = EqSet []

member :: Eq a => a -> EqSet a -> Bool
member x (EqSet xs) = x `elem` xs

insert :: Eq a => a -> EqSet a -> EqSet a
insert x s@(EqSet xs)
  | x `member` s = s
  | otherwise    = EqSet (x:xs)

remove :: Eq a => a -> EqSet a -> EqSet a
remove x (EqSet xs) = EqSet (filter (/= x) xs)

elems :: EqSet a -> [a]
elems (EqSet xs) = xs

instance Eq a => Eq (EqSet a) where
  (EqSet xs) == (EqSet ys) = null (listDifference xs ys) && null (listDifference ys xs)
    where
      listDifference = foldl (flip delete)

instance Show a => Show (EqSet a) where
  show (EqSet xs) = "{" ++ init (concatMap ((++ ",") . show) xs) ++ "}"

instance Eq a => Semigroup (EqSet a) where
  (EqSet xs) <> (EqSet ys) = EqSet (listUnion xs ys)
    where
      listUnion a b = nub (a ++ b)

instance Eq a => Monoid (EqSet a) where
  mempty = empty
