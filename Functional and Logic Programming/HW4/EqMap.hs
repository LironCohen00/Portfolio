{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module EqMap (
  EqMap,
  CombiningMap (..),
  empty,
  EqMap.insert, -- To avoid name clash with Data.List.insert
  member,
  remove,
  EqMap.lookup, -- To avoid name clash with Prelude.lookup
  assocs
) where

import Data.Either
import Data.List
import Data.Maybe
import Data.Semigroup (Arg (..))
import EqSet (EqSet)
import qualified EqSet as ES

newtype EqMap k v = EqMap (EqSet (Arg k v))

empty :: EqMap k v
empty = EqMap ES.empty

member :: Eq k => k -> EqMap k v -> Bool
member key (EqMap es) = ES.member (Arg key undefined) es

insert :: Eq k => k -> v -> EqMap k v -> EqMap k v
insert key val (EqMap es) = EqMap $ ES.insert (Arg key val) $ ES.remove (Arg key undefined) es

remove :: Eq k => k -> EqMap k v -> EqMap k v
remove key (EqMap es) = EqMap $ ES.remove (Arg key undefined) es

lookup :: Eq k => k -> EqMap k v -> Maybe v
lookup key (EqMap es) = (\(Arg _ v) -> v) <$> find (\(Arg k _) -> k == key) (ES.elems es)

assocs :: EqMap k v -> [(k, v)]
assocs (EqMap es) = map (\(Arg k v) -> (k, v)) (ES.elems es)

instance (Eq k, Eq v) => Eq (EqMap k v) where
  (==) :: (Eq k, Eq v) => EqMap k v -> EqMap k v -> Bool
  EqMap es1 == EqMap es2 = es1 == es2

instance (Show k, Show v) => Show (EqMap k v) where
  show :: (Show k, Show v) => EqMap k v -> String
  show (EqMap es) = "{" ++ intercalate "," (map showPair (ES.elems es)) ++ "}"
    where
      showPair (Arg k v) = show k ++ "->" ++ show v

instance Eq k => Semigroup (EqMap k v) where
  (<>) :: Eq k => EqMap k v -> EqMap k v -> EqMap k v
  EqMap es1 <> EqMap es2 = foldr (uncurry EqMap.insert) (EqMap es1) (assocs (EqMap es2))


instance Eq k => Monoid (EqMap k v) where
  mempty :: Eq k => EqMap k v
  mempty = empty

newtype CombiningMap k v = CombiningMap { getCombiningMap :: EqMap k v }


instance (Eq k, Semigroup v) => Semigroup (CombiningMap k v) where
  (<>) :: (Eq k, Semigroup v) => CombiningMap k v -> CombiningMap k v -> CombiningMap k v
  CombiningMap em1 <> CombiningMap em2 = CombiningMap $ foldr insertWithComb em1 (assocs em2)
    where
      insertWithComb (k, v) em =
        let combinedValue = case EqMap.lookup k em of
                              Just oldValue -> oldValue <> v
                              Nothing -> v
        in EqMap.insert k combinedValue em
instance (Eq k, Semigroup v) => Monoid (CombiningMap k v) where
  mempty :: (Eq k, Semigroup v) => CombiningMap k v
  mempty = CombiningMap empty
