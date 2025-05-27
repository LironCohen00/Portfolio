{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW2 where

import Data.List (find, foldl')
import Prelude (Bool (..), Bounded (..), Char, Either (..), Enum (..), Eq (..), Int, Integer, Maybe (..), Num (..), Ord (..), Show (..), String, all, and, any, concat, concatMap, const, curry, div, elem, error, even, filter, flip, foldl, foldr, fst, id, length, lines, lookup, map, mod, not, notElem, null, odd, otherwise, product, snd, sum, uncurry, undefined, unlines, unwords, words, (!!), ($), (&&), (++), (.), (||))

------------------------------------------------
-- DO NOT MODIFY ANYTHING ABOVE THIS LINE !!! --
------------------------------------------------

-- Section 1.1: Basic Maybes
concatMaybeMap :: (a -> Maybe b) -> Maybe a -> Maybe b
concatMaybeMap f a = case a of
    Nothing -> Nothing
    Just x -> f x 
fromMaybe :: a -> Maybe a -> a
fromMaybe a ma = case ma of
    Nothing -> a
    Just x -> x
maybe :: b -> (a -> b) -> Maybe a -> b
maybe b f ma = case ma of
    Nothing -> b
    Just x -> f x
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (ma : tail) = case ma of
    Nothing -> catMaybes tail
    Just x -> x : (catMaybes tail)
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (a : a_tail) = case (f a) of
    Nothing -> mapMaybe f a_tail
    Just x -> x : (mapMaybe f a_tail)
-- Section 1.2 Basic Eithers
concatEitherMap :: (a -> Either e b) -> Either e a -> Either e b
concatEitherMap _ (Left e) = Left e
concatEitherMap f (Right a) = f a
either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left a) = f a
either _ g (Right b) = g b
mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left (f a)
mapLeft _ (Right b) = Right b

--containsLeft :: [Either e a] -> Bool
--containsLeft [] = False
--containsLeft (Right _ : tail) = containsLeft tail
--containsLeft (Left _ : _) = True

transformEitherList :: [Either e a] -> [a]
transformEitherList (Right a : tail) = [a] ++ transformEitherList tail
transformEitherList _ = []

catEithersHelper :: [Either e a] -> Either e [a]
catEithersHelper [] = Right []
catEithersHelper (Left e : _) = Left e
catEithersHelper (Right _ : tail) = catEithersHelper tail 

catEithers :: [Either e a] -> Either e [a]
catEithers [] = Right []
catEithers myList = if (length transformedList /= length myList) then (catEithersHelper myList) else Right transformedList
    where
        transformedList = transformEitherList myList

transformEitherListWithMapping :: (a -> Either e b) -> [a] -> [Either e b]
transformEitherListWithMapping _ [] = []
transformEitherListWithMapping f (a : tail) = [f a] ++ transformEitherListWithMapping f tail

mapEither :: (a -> Either e b) -> [a] -> Either e [b] 
mapEither _ [] = Right []
mapEither f myList = catEithers (transformEitherListWithMapping f myList)
partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers [] = ([],[])
partitionEithers (x : tail) = (filter_a (x : tail), filter_b (x : tail))

filter_a :: [Either a b] -> [a]
filter_a [] = []
filter_a (Left a : tail) = (a : (filter_a tail))
filter_a (Right _ : tail) = filter_a tail

filter_b :: [Either a b] -> [b]
filter_b [] = []
filter_b (Left _ : tail) = filter_b tail
filter_b (Right b : tail) = (b : (filter_b tail)) 
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right b) = Just b

-- Section 2: Lists
take :: Int -> [a] -> [a]
take 0 (_ : _) = []
take _ [] = []
take n (a : tail) = (a : (take (n-1) tail))
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (a : tail) = if (f a) then (a : (takeWhile f tail)) else []
drop :: Int -> [a] -> [a]
drop _ [] = []
drop 0 (a : tail) = (a : tail)
drop n (_ : tail) = drop (n-1) tail
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile f (a : tail) = if (f a) then (dropWhile f tail) else (a : tail)


reverse :: [a] -> [a]
reverse [] = []
reverse myList = drop (penultimateIndex) myList ++ (reverse (take (penultimateIndex) myList))
    where
        penultimateIndex = (length myList) - 1
rotate :: Int -> [a] -> [a]
rotate n myList 
    | n <= 0    = myList
    | otherwise = rotate (n-1) listRotatedByOne
    where
        penultimateIndex = (length myList) - 1
        listRotatedByOne = drop penultimateIndex myList ++ take penultimateIndex myList
lotate :: Int -> [a] -> [a]
lotate _ [] = []
lotate n (a : tail) 
    | n <= 0    = (a : tail) 
    | otherwise = lotate (n-1) listLotatedByOne
    where
        listLotatedByOne = tail ++ [a]
type Generator a = (a -> a, a -> Bool, a)
fromGenerator :: Generator a -> [a]
fromGenerator (f, g, a) = if g a == True then [f a] ++ fromGenerator (f, g, f a) else []
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n a = [a] ++ replicate (n-1) a
inits :: [a] -> [[a]]
inits [] = [[]]
inits (a : tail) = inits (take (length (a : tail) - 1) (a : tail)) ++ [(a : tail)]
tails :: [a] -> [[a]]
tails [] = [[]]
tails (a : tail) = [(a : tail)] ++ tails tail

-- Section 3: zips and products
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (a : []) (b : _) = [f a b]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (a : _) (b : []) = [f a b]
zipWith f (a : a_tail) (b : b_tail) = [f a b] ++ zipWith f a_tail b_tail
zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (a : []) (b : _) = [(a,b)]
zip (a : _) (b : []) = [(a,b)]
zip (a : a_tail) (b : b_tail) = [(a,b)] ++ zip a_tail b_tail
zipFill :: a -> b -> [a] -> [b] -> [(a, b)]
zipFill _ _ [] [] = []
zipFill a0 b0 [] (b : tail) = [(a0,b)] ++ zipFill a0 b0 [] tail
zipFill a0 b0 (a : tail) [] = [(a,b0)] ++ zipFill a0 b0 tail []
zipFill a_0 b_0 (a : a_tail) (b : b_tail) = [(a,b)] ++ zipFill a_0 b_0 a_tail b_tail
data ZipFail = ErrorFirst | ErrorSecond deriving (Eq, Show)
zipFail :: [a] -> [b] -> Either ZipFail [(a, b)]
zipFail listA listB = case (checkEqualLength listA listB) of
    Right True  -> Right (zip listA listB)
    Left ErrorFirst -> Left ErrorFirst
    Left ErrorSecond -> Left ErrorSecond
    Right False -> Left ErrorFirst -- This is not a value that checkEqualLength can output, but i have to add the case to avoid "non-exhaustive pattern matching" error
checkEqualLength :: [a] -> [b] -> Either ZipFail Bool
checkEqualLength [] [] = Right True
checkEqualLength (_ : tailA) (_ : tailB) = checkEqualLength tailA tailB
checkEqualLength [] (_ : _) = Left ErrorFirst
checkEqualLength (_ : _) [] = Left ErrorSecond
unzip :: [(a, b)] -> ([a], [b])
unzip [] = ([], [])
unzip ((a,b) : []) = ([a], [b])
unzip ((a,b) : tail) = concatTupleOfLists ([a] , [b])  (unzip tail)

concatTupleOfLists :: ([a], [b]) -> ([a], [b]) -> ([a] , [b])
concatTupleOfLists (listA , listB) (listC, listD) = (listA ++ listC , listB ++ listD)

-- Section 4: Knight travels
-- Position (0, 0) is the top-left corner.
data KnightPos = KnightPos {x :: Int, y :: Int} deriving (Show, Eq)
data KnightMove = TopLeft | TopRight | RightTop | RightBottom | BottomRight | BottomLeft | LeftBottom | LeftTop deriving (Enum, Bounded, Show, Eq)
-- Utility to get all knight moves. Don't worry about the implementation of this.
allKnightMoves :: [KnightMove]
allKnightMoves = [minBound .. maxBound]
data Board = Board {width :: Int, height :: Int} deriving (Show, Eq)
tour :: Board -> KnightPos -> Maybe [KnightMove]
tour = undefined
newtype InvalidPosition = InvalidPosition KnightPos deriving (Show, Eq)
translate :: KnightPos -> [KnightMove] -> [KnightPos]
translate _ [] = []
translate (KnightPos x y) (move : tail)
    | move == TopLeft       = [KnightPos (x-2) (y-1)] ++ translate (KnightPos (x-2) (y-1)) tail
    | move == TopRight      = [KnightPos (x+2) (y-1)] ++ translate (KnightPos (x+2) (y-1)) tail
    | move == RightTop      = [KnightPos (x+1) (y-2)] ++ translate (KnightPos (x+1) (y-2)) tail
    | move == RightBottom   = [KnightPos (x+1) (y+2)] ++ translate (KnightPos (x+1) (y+2)) tail
    | move == BottomRight   = [KnightPos (x+2) (y+1)] ++ translate (KnightPos (x+2) (y+1)) tail
    | move == BottomLeft    = [KnightPos (x-2) (y+1)] ++ translate (KnightPos (x-2) (y+1)) tail
    | move == LeftBottom    = [KnightPos (x-1) (y+2)] ++ translate (KnightPos (x-1) (y+2)) tail
    | move == LeftTop       = [KnightPos (x-1) (y-2)] ++ translate (KnightPos (x-1) (y-2)) tail
    | otherwise             = []   -- i had to include this or i would get a "non-exhaustive pattern matching" warning
translate' :: [KnightPos] -> Either InvalidPosition [KnightMove]

translate'Helper :: [KnightPos] -> [KnightMove]
translate'Helper [] = []
translate'Helper ((KnightPos _ _) : []) = []
translate'Helper ((KnightPos x y) : (KnightPos nextX nextY) : tail)
    | x-2 == nextX && y-1 == nextY      = [TopLeft] ++ translate'Helper ((KnightPos nextX nextY) : tail) 
    | x+2 == nextX && y-1 == nextY      = [TopRight] ++ translate'Helper ((KnightPos nextX nextY) : tail) 
    | x+1 == nextX && y-2 == nextY      = [RightTop] ++ translate'Helper ((KnightPos nextX nextY) : tail) 
    | x+1 == nextX && y+2 == nextY      = [RightBottom] ++ translate'Helper ((KnightPos nextX nextY) : tail) 
    | x+2 == nextX && y+1 == nextY      = [BottomRight] ++ translate'Helper ((KnightPos nextX nextY) : tail) 
    | x-2 == nextX && y+1 == nextY      = [BottomLeft] ++ translate'Helper ((KnightPos nextX nextY) : tail) 
    | x-1 == nextX && y+2 == nextY      = [LeftBottom] ++ translate'Helper ((KnightPos nextX nextY) : tail) 
    | x-1 == nextX && y-2 == nextY      = [LeftTop] ++ translate'Helper ((KnightPos nextX nextY) : tail) 
    | otherwise                         = []

translate' posList
    | length movesList == (length posList - 1)  = Right movesList
    | otherwise                                 = Left (InvalidPosition (posList !! (length movesList + 1)))
    where 
        movesList = translate'Helper posList
-- Bonus (10 points)
mark :: Board -> [KnightPos] -> Either InvalidPosition [[Int]]
mark = undefined
