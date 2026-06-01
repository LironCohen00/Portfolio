{-# LANGUAGE LambdaCase #-}
-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW2.hs should successfully compile.
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW3 where

import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Int, Integer, Maybe (..), Num (..), Ord (..), Rational, Show (..), String, all, and, any, concat, concatMap, const, curry, div, drop, dropWhile, elem, error, even, filter, flip, foldr, fst, id, init, last, length, lines, lookup, map, maximum, minimum, mod, not, notElem, null, odd, or, otherwise, product, reverse, snd, splitAt, sum, tail, take, takeWhile, uncurry, undefined, unlines, unwords, unzip, words, zip, zipWith, (!!), ($), (&&), (++), (.), (||))

import Data.Either (either, fromLeft, fromRight, isLeft, isRight, lefts, partitionEithers, rights)
import Data.List (find, foldl', isInfixOf, isPrefixOf, isSuffixOf, nub, uncons)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybe)
import Data.Ratio (denominator, numerator, (%))
import Text.Read (readMaybe)
import GHC.Float (divideDouble)

-- Section 1: Tree Serialization
data Tree a = Empty | Tree (Tree a) a (Tree a) deriving (Show, Eq)
serialize :: Tree Int -> [Int]
serialize = undefined
deserialize :: [Int] -> Tree Int
deserialize = undefined
-- Section 2: Infinite lists
data InfiniteList a = a :> InfiniteList a
infixr 5 :>

sample :: InfiniteList a -> [a]
sample = take 10 . itoList
smallSample :: InfiniteList a -> [a]
smallSample = take 5 . itoList
itoList :: InfiniteList a -> [a]
itoList (a :> a_tail) = a : itoList a_tail
iiterate :: (a -> a) -> a -> InfiniteList a
iiterate f a = a :> iiterate f (f a)
irepeat :: a -> InfiniteList a
irepeat a = a :> irepeat a
iprepend :: [a] -> InfiniteList a -> InfiniteList a
iprepend [] (a_i :> tail_i) = a_i :> tail_i
iprepend (a : a_tail) infList = a :> iprepend a_tail infList
itake :: Integer -> InfiniteList a -> [a]
itake 0 _ = []
itake n (a :> a_tail) = a : itake (n-1) a_tail
idrop :: Integer -> InfiniteList a -> InfiniteList a
idrop 0 list = list
idrop n (_ :> a_tail) = idrop (n-1) a_tail
naturals :: InfiniteList Integer
naturals = naturals' 0
naturals' :: Integer -> InfiniteList Integer
naturals' n = n :> naturals' (n+1)
imap :: (a -> b) -> InfiniteList a -> InfiniteList b
imap f (a :> a_tail) = f a :> imap f a_tail
ifilter :: (a -> Bool) -> InfiniteList a -> InfiniteList a
ifilter f (a :> a_tail) = if f a then a :> ifilter f a_tail else ifilter f a_tail
ifind :: (a -> Bool) -> InfiniteList a -> a
ifind f (a :> a_tail) = if f a then a else ifind f a_tail
iconcat :: InfiniteList [a] -> InfiniteList a
iconcat ([] :> i_tail) = iconcat i_tail
iconcat ([a] :> i_tail) = a :> iconcat i_tail
iconcat ((a : a_tail) :> i_tail) = a :> iconcat (a_tail :> i_tail)
integers :: InfiniteList Integer
integers = integers' 0
integers' :: Integer -> InfiniteList Integer
integers' 0 = 0 :> integers' 1
integers' n = n :> -n :> integers' (n+1)
rationals :: InfiniteList Rational
rationals = generate 1 1
  where
    generate p q = (p % q) :> negate (p % q) :> generateNext p q
    generateNext p q
      | p == 1    = generate (q + 1) 1  -- Move to the next row in the diagonal traversal
      | otherwise = generate (p - 1) (q + 1)
-- Bonus: same as rationals, but without repeats!
rationals' :: InfiniteList Rational
rationals' = undefined


-- Section 3: Stack Machine
data StackError = DivisionByZero | StackUnderflow {instruction :: String, stackValue :: Maybe Int} deriving (Show, Eq)
data RunError = InstructionError StackError | ParseError {line :: String} deriving (Show, Eq)
data Instruction = PUSH Int | POP | SWAP | DUP | ADD | SUB | MUL | DIV
parseAndRun :: String -> Either RunError [Int]
parseAndRun str = runInstructions [] (parseInstructions (lines str))

parseInstructions :: [String] -> [Either RunError Instruction]
parseInstructions [] = []
parseInstructions (str : i_tail) = case str of
    ""          ->  parseInstructions i_tail
    "POP"       ->  Right POP : parseInstructions i_tail
    "SWAP"      ->  Right SWAP : parseInstructions i_tail
    "DUP"       ->  Right DUP : parseInstructions i_tail
    "ADD"       ->  Right ADD : parseInstructions i_tail
    "SUB"       ->  Right SUB : parseInstructions i_tail
    "MUL"       ->  Right MUL : parseInstructions i_tail
    "DIV"       ->  Right DIV : parseInstructions i_tail
    maybePush   ->  parsePushInstruction maybePush : parseInstructions i_tail

parsePushInstruction :: String -> Either RunError Instruction
parsePushInstruction str = if "PUSH " `isPrefixOf` str then 
    (case num of
    Just n -> Right (PUSH n)
    _ -> Left (ParseError {line = str})) 
    else Left (ParseError {line = str})
    where num = readMaybe (drop 5 str)

runInstructions :: [Int] -> [Either RunError Instruction] -> Either RunError [Int]
runInstructions stack [] = Right stack
runInstructions _ (Left parseError : _) = Left parseError
runInstructions stack (Right instruction : i_tail) = case result of
    Right newStack  -> runInstructions newStack i_tail
    _               -> Left (InstructionError (fromLeft DivisionByZero result))
    where result = runInstruction stack instruction

runInstruction :: [Int] -> Instruction -> Either StackError [Int]
runInstruction [] (PUSH n) = Right [n]
runInstruction [] instruction = Left (StackUnderflow {instruction = toString instruction, stackValue = Nothing})
runInstruction [a] instruction = case instruction of
    POP         -> Right []
    DUP         -> Right [a,a]
    (PUSH n)    -> Right [n, a]
    _   -> Left StackUnderflow {instruction = toString instruction, stackValue = Just a}

runInstruction (a : b : i_tail) instruction = case instruction of
    POP         -> Right (b : i_tail)
    SWAP        -> Right (b : a : i_tail)
    DUP         -> Right (a : a : b : i_tail)
    ADD         -> Right (a + b : i_tail)
    SUB         -> Right (a - b : i_tail)
    MUL         -> Right (a * b : i_tail)
    DIV         -> if b == 0 then Left DivisionByZero else Right (a `div` b : i_tail)
    (PUSH n)    -> Right (n : a : b : i_tail)

toString :: Instruction -> String
toString (PUSH n) = "PUSH " ++ show n
toString POP = "POP"
toString SWAP = "SWAP"
toString DUP = "DUP"
toString ADD = "ADD"
toString SUB = "SUB"
toString MUL = "MUL"
toString DIV = "DIV"
