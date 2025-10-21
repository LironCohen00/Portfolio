{-# LANGUAGE LambdaCase #-}
-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW4.hs EqSet.hs EqMap.hs should successfully compile.
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
--{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW4 where

import Data.Char (chr, ord)
import Data.Either
import Data.List
import Data.Maybe
import Data.Semigroup (Arg (..))
import EqMap (EqMap)
import EqMap qualified
import EqSet (EqSet)
import EqSet qualified

-- Section 2: Serialization
class Serializable a where
  serialize :: a -> [Int]
  deserialize :: [Int] -> a

instance Serializable Int where
  serialize a = [a]
  deserialize [a] = a
instance Serializable Bool where
  serialize True = [1]
  serialize False = [0]
  deserialize [1] = True
  deserialize [0] = False
instance Serializable Char where
  serialize a = [ord a]
  deserialize [a] = chr a
instance Serializable a => Serializable (Maybe a) where
  serialize Nothing = []
  serialize (Just x) = serialize x
  deserialize [] = Nothing
  deserialize arr = Just (deserialize arr)
instance (Serializable a, Serializable b) => Serializable (a, b) where
  serialize (a,b) = length serializedA : serializedA ++ serializedB
    where
      serializedA = serialize a
      serializedB = serialize b
  deserialize  (a : a_tail) = (deserialize serializedA, deserialize serializedB)
    where
      (serializedA , serializedB) = splitAt a a_tail
instance (Serializable a, Serializable b) => Serializable (Either a b) where
  serialize (Left a) = 0 : serialize a
  serialize (Right b) = 1 : serialize b
  deserialize (0 : a_tail) = Left (deserialize a_tail)
  deserialize (1 : a_tail) = Right (deserialize a_tail)
instance Serializable a => Serializable [a] where
  serialize (a : a_tail) = length serializedA : serialize a ++ serialize a_tail
    where
      serializedA = serialize a
  serialize [] = []
  deserialize (a : a_tail) = deserialize serializedA : deserialize remainingArr
    where
      (serializedA , remainingArr) = splitAt a a_tail
  deserialize [] = []
instance (Serializable a, Eq a) => Serializable (EqSet a) where
  serialize eqSet = serialize (EqSet.elems eqSet)
  deserialize arr = buildEqSet EqSet.empty (deserialize arr)

buildEqSet :: Eq a => EqSet a -> [a] -> EqSet a
buildEqSet = foldl (flip EqSet.insert)
instance (Serializable k, Eq k, Serializable v) => Serializable (EqMap k v) where
  serialize _ = undefined
  deserialize _ = undefined

-- Section 3: Metric
infinity :: Double
infinity = 1 / 0

class Eq a => Metric a where
  distance :: a -> a -> Double

instance Metric Double where
  distance a b = abs (a-b)
instance Metric Int where
  distance a b = abs (fromIntegral (a-b))
instance Metric Char where
  distance a b = abs $ fromIntegral (ord a - ord b)

-- Euclidean distance
instance (Metric a, Metric b) => Metric (a, b) where
  distance (a_1, b_1) (a_2, b_2) = sqrt (square (distance a_1 a_2) + square (distance b_1 b_2))

square :: Double -> Double
square a = a * a

data ManhattanTuple a b = ManhattanTuple a b deriving Eq
instance (Metric a, Metric b) => Metric (ManhattanTuple a b) where
   distance (ManhattanTuple a_1 b_1) (ManhattanTuple a_2 b_2) = abs (distance a_1 a_2 + distance b_1 b_2)


-- Just and Nothing have distance of infinity.
-- Two Justs measure the distance between the two values.
instance Metric a => Metric (Maybe a) where
  distance _ Nothing = infinity
  distance Nothing _ = infinity
  distance (Just x) (Just y) = distance x y

-- Left and Right have a distance of infinity.
-- Same constructores measure the distance between the two values.
instance (Metric a, Metric b) => Metric (Either a b) where
  distance (Left _) (Right _) = infinity
  distance (Right _) (Left _) = infinity
  distance (Left x) (Left y) = distance x y
  distance (Right x) (Right y) = distance x y

-- Lists of different sizes have distance of infinity.
-- Euclidean distance.
instance Metric a => Metric [a] where
  distance arr1 arr2
    | length (arr1) /= length (arr2)  = infinity
    | otherwise                     = sqrt (totalDistance arr1 arr2)
    where
      totalDistance (x : x_tail) (y : y_tail) = square (distance x y) + totalDistance x_tail y_tail
      totalDistance [] [] = 0


newtype ManhattanList a = ManhattanList [a] deriving Eq
instance Metric a => Metric (ManhattanList a) where
  distance (ManhattanList arr1) (ManhattanList arr2)
    | length (arr1) /= length (arr2)  = infinity
    | otherwise                     = totalDistance arr1 arr2
    where
      totalDistance (x : x_tail) (y : y_tail) = distance x y + totalDistance x_tail y_tail
      totalDistance [] [] = 0

-- Returns the element with the shortest distance to the input.
-- If there are no numbers whose distance is less than infinity, return Nothing.
closest :: Metric a => a -> [a] -> Maybe a
closest x (a : a_tail) = if distance result x == infinity then Nothing else Just result
  where
    result = closestHelper x a a_tail

closestHelper :: Metric a => a -> a -> [a] -> a
closestHelper x closestElem [] = closestElem
closestHelper x closestElem (a : a_tail) = if dist < minSoFar then closestHelper x a a_tail else closestHelper x closestElem a_tail
  where
    dist = distance x a
    minSoFar = distance x closestElem

-- Similar to the above, but uses a function move the element
-- to another metric space.
closestOn :: Metric b => (a -> b) -> a -> [a] -> Maybe a
closestOn f x (a : a_tail) = if distance (f result) (f x) == infinity then Nothing else Just result
  where
    result = closestOnHelper f x a a_tail

closestOnHelper :: Metric b => (a -> b) -> a -> a -> [a] -> a
closestOnHelper f x closestElem [] = closestElem
closestOnHelper f x closestElem (a : a_tail) = if dist < minSoFar then closestOnHelper f x a a_tail else closestOnHelper f x closestElem a_tail
  where
    dist = distance (f x) (f a)
    minSoFar = distance (f x) (f closestElem)


-- Will not swap elements whose distance is less than d, even if their
-- order implies they should be swapped.
metricBubbleSort :: (Metric a, Ord a) => Double -> [a] -> [a]
metricBubbleSort d arr = metricBubbleSortHelper d arr (length arr)
metricBubbleSortHelper :: (Metric a, Ord a) => Double -> [a] -> Int -> [a]
metricBubbleSortHelper d arr 0 = arr
metricBubbleSortHelper d arr numIter = metricBubbleSortHelper d (metricBubbleSortIteration d arr) (numIter-1)
metricBubbleSortIteration :: (Metric a, Ord a) => Double -> [a] -> [a]
metricBubbleSortIteration d [a] = [a]
metricBubbleSortIteration d (a : b : a_tail) = if a < b && (distance a b >= d) then b : metricBubbleSortIteration d (a : a_tail) else a : metricBubbleSort d (b : a_tail)
-- Similar to the above, but uses a function to extract the value used for sorting.
metricBubbleSortOn :: (Metric b, Ord b) => (a -> b) -> Double -> [a] -> [a]
metricBubbleSortOn f d [a] = [a]
metricBubbleSortOn f d (a : b : a_tail) = if f a < f b && (distance (f a) (f b) >= d) then b : metricBubbleSortOn f d (a : a_tail) else a : metricBubbleSortOn f d (b : a_tail)

-- Bonus (10 points).
clusters :: Metric a => [a] -> [[a]]
clusters = undefined
