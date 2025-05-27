{-# LANGUAGE LambdaCase #-}
-- Implement the following functions.
-- When you're done, ghc -Wall -Werror Deque.hs HW5.hs
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW5 where

import Control.Applicative (liftA2)
import Data.Char (chr, ord, toLower, toUpper)
import Data.Either
import Data.List (foldl', uncons)
import Data.Map (Map, (!?))
import Data.Map qualified as M
import Data.Maybe
import Data.Monoid (All (..), Any (..), First (..), Last (..), Product (..), Sum (..))
import Data.Ord (Down (..))
import Data.Semigroup (Arg (..), Max (..), Min (..))
import Data.Set (Set)
import Data.Set qualified as S
import Deque (Deque)
import Deque qualified as DQ

data FoldMapFunc a m result = FoldMapFunc {agg :: a -> m, finalize :: m -> result}

foldMap' :: (Foldable t, Monoid m) => FoldMapFunc a m result -> t a -> result
foldMap' FoldMapFunc{agg, finalize} = finalize . foldMap agg

-- Section 1: Foldable functions
fmsum :: Num a => FoldMapFunc a (Sum a) a
fmsum = FoldMapFunc Sum getSum
fmor :: FoldMapFunc Bool Any Bool
fmor = FoldMapFunc Any getAny
fmfold :: Monoid a => FoldMapFunc a a a
fmfold = FoldMapFunc id id
fmelem :: Eq a => a -> FoldMapFunc a Any Bool
fmelem a = FoldMapFunc (isEqualAnyMonoid a) getAny
isEqualAnyMonoid :: Eq a => a -> a -> Any
isEqualAnyMonoid a x = Any (a == x)
fmfind :: (a -> Bool) -> FoldMapFunc a (First a) (Maybe a)
fmfind f = FoldMapFunc (\ x -> if f x then First (Just x) else First Nothing) getFirst
fmlength :: FoldMapFunc a (Sum Int) Int
fmlength = FoldMapFunc (\ _ -> Sum 1) getSum
fmnull :: FoldMapFunc a (Sum Int) Bool
fmnull = FoldMapFunc (\ _ -> Sum 1) (\ (Sum a) -> a == 0)

fmmaximum :: Ord a => FoldMapFunc a (Maximum a) (Maybe a)
fmmaximum = FoldMapFunc (Maximum . Just) getMaximum
newtype Maximum a = Maximum {getMaximum :: Maybe a}
instance Ord a => Semigroup (Maximum a) where
    Maximum Nothing <> b = b
    a <> Maximum Nothing = a
    Maximum (Just x) <> Maximum (Just y) = Maximum (Just (max x y))
instance Ord a => Monoid (Maximum a) where
    mempty = Maximum Nothing
    mappend = (<>)

fmminimum :: Ord a => FoldMapFunc a (Minimum a) (Maybe a)
fmminimum = FoldMapFunc (Minimum . Just) getMinimum
newtype Minimum a = Minimum {getMinimum :: Maybe a}
instance Ord a => Semigroup (Minimum a) where
    Minimum Nothing <> b = b
    a <> Minimum Nothing = a
    Minimum (Just x) <> Minimum (Just y) = Minimum (Just (min x y))
instance Ord a => Monoid (Minimum a) where
    mempty = Minimum Nothing
    mappend = (<>)

fmmaxBy :: Ord b => (a -> b) -> FoldMapFunc a (MaxBy a b) (Maybe a)
fmmaxBy f = FoldMapFunc (\ x -> MaxBy (Just f) (Just x)) getMaxBy
data MaxBy a b = MaxBy {mapping :: Maybe (a -> b), getMaxBy :: Maybe a}
instance Ord b => Semigroup (MaxBy a b) where
    MaxBy _ Nothing <> b = b
    a <> MaxBy _ Nothing = a
    MaxBy (Just f) (Just x) <> MaxBy (Just g) (Just y) = if f x >= g y then MaxBy (Just f) (Just x) else MaxBy (Just g) (Just y)
    MaxBy Nothing (Just _) <> b = b
    a <> MaxBy Nothing (Just _) = a
instance Ord b => Monoid (MaxBy a b) where
    mempty :: Ord b => MaxBy a b
    mempty = MaxBy Nothing Nothing
    mappend :: Ord b => MaxBy a b -> MaxBy a b -> MaxBy a b
    mappend = (<>)
fmminBy :: Ord b => (a -> b) -> FoldMapFunc a (MinBy a b) (Maybe a)
fmminBy f = FoldMapFunc (\ x -> MinBy (Just f) (Just x)) getMinBy
data MinBy a b = MinBy {mapping2 :: Maybe (a -> b), getMinBy :: Maybe a}
instance Ord b => Semigroup (MinBy a b) where
    MinBy _ Nothing <> b = b
    a <> MinBy _ Nothing = a
    MinBy (Just f) (Just x) <> MinBy (Just g) (Just y) = if f x <= g y then MinBy (Just f) (Just x) else MinBy (Just g) (Just y)
    MinBy Nothing (Just _) <> b = b
    a <> MinBy Nothing (Just _) = a
instance Ord b => Monoid (MinBy a b) where
    mempty = MinBy Nothing Nothing
    mappend = (<>)
fmtoList :: FoldMapFunc a (ToList a) [a]
fmtoList = FoldMapFunc (\x -> ToList [x]) getList
newtype ToList a = ToList {getList :: [a]}
instance Semigroup (ToList a) where
    ToList list1 <> ToList list2 = ToList (list1 ++ list2)
instance Monoid (ToList a) where
    mempty = ToList []
    mappend = (<>)

-- Section 2: Deque instances (Don't forget to implement the instances in Deque.hs as well!)
newtype DequeWrapper a = DequeWrapper (Deque a) deriving (Show, Eq)
instance Semigroup (DequeWrapper a) where
  (<>) :: DequeWrapper a -> DequeWrapper a -> DequeWrapper a
  (DequeWrapper x) <> (DequeWrapper y) = case DQ.popl y of
    Just (n, deque) -> DequeWrapper (DQ.pushr n x) <> DequeWrapper deque
    Nothing -> DequeWrapper x
instance Monoid (DequeWrapper a) where
  mempty = DequeWrapper DQ.empty
  mappend = (<>)
instance Foldable DequeWrapper where
  foldMap f (DequeWrapper x) = case DQ.popl x of
    Just (n, deque) -> mappend (f n) (foldMap f (DequeWrapper deque))
    Nothing -> mempty
instance Functor DequeWrapper where
  fmap mapping (DequeWrapper x) = case DQ.popl x of
    Just (n, deque) -> DequeWrapper (DQ.pushr (mapping n) DQ.empty) <> fmap mapping (DequeWrapper deque)
    Nothing -> mempty
instance Applicative DequeWrapper where
  pure a = DequeWrapper (DQ.pushr a DQ.empty)
  liftA2 f (DequeWrapper x) y = case DQ.popl x of
    Just (n, deque) -> fmap (f n) y <> liftA2 f (DequeWrapper deque) y
    Nothing -> mempty
instance Monad DequeWrapper where
    (DequeWrapper x) >>= f = case DQ.popl x of
      Just (n, deque) -> f n <> (>>=) (DequeWrapper deque) f
      Nothing -> mempty

-- Section 3: Calculator and traverse
class Monad f => CalculatorError f where
  divideByZero :: f Int
  missingVariable :: String -> f Int

runCalculator :: CalculatorError f => Map String Int -> Expr -> f Int
runCalculator vars = eval
  where
    eval (Var x) = case M.lookup x vars of
        Nothing -> missingVariable x
        Just v  -> return v
    eval (Val n) = return n
    eval (Add e1 e2) = do
        v1 <- eval e1
        v2 <- eval e2
        return (v1 + v2)
    eval (Sub e1 e2) = do
        v1 <- eval e1
        v2 <- eval e2
        return (v1 - v2)
    eval (Mul e1 e2) = do
        v1 <- eval e1
        v2 <- eval e2
        return (v1 * v2)
    eval (Div e1 e2) = do
        v1 <- eval e1
        v2 <- eval e2
        if v2 == 0
            then divideByZero
            else return (v1 `div` v2)

-- Instances to implement:
instance CalculatorError Maybe where
  divideByZero = Nothing
  missingVariable _ = Nothing

data Err = DivByZero | MissingVar String deriving (Show, Eq)
instance CalculatorError (Either Err) where
  divideByZero = Left DivByZero
  missingVariable var = Left (MissingVar var)

data Defaults
  = Defaults
  -- This replaces the entire division result, not just the denominator!
  { defaultForDivisionByZero :: Int
  , defaultForVariable :: String -> Int
  }
instance CalculatorError (Reader Defaults) where
  divideByZero = Reader defaultForDivisionByZero
  missingVariable var = Reader (`defaultForVariable` var)

-- From the lectures:
newtype Reader r a = Reader {runReader :: r -> a}
instance Functor (Reader r) where
  fmap f r = Reader $ f . runReader r
instance Applicative (Reader r) where
  pure = Reader . const
  liftA2 f ra rb = Reader $ \r -> f (runReader ra r) (runReader rb r)
instance Monad (Reader r) where
  ra >>= f = Reader $ \r -> runReader (f $ runReader ra r) r

data Expr
  = Val Int
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

-- Section 4: Hangman

hangman :: String -> IO Int
hangman word = gameLoop word [] 0 "Guess a letter: "

gameLoop :: String -> [Char] -> Int -> String -> IO Int
gameLoop word guesses count message = do
    putStrLn (displayWord word guesses)
    putStr message
    guess <- getChar
    _ <- getChar -- to consume the newline character
    if toLower guess `elem` map toLower word then
        if all (`elem` (map toLower (' ' : guess:guesses))) (map toLower word) then
            do
            putStrLn "Very good, the word is:"
            putStrLn word
            return ((count + 1) - countUniqueLetters word)
        else
          do
          if toLower guess `elem` map toLower guesses then
            gameLoop word guesses count "Guess a letter: "
          else
            gameLoop word (toLower guess : guesses) (count + 1) "Guess a letter: "
    else if ord (toUpper guess) < 65 || ord (toUpper guess) > 90 then
        do
        putStrLn ("Invalid letter guess " ++ [guess] ++ "!")
        gameLoop word guesses count "Try again: "

    else
        do
        putStrLn "Wrong guess!"
        gameLoop word (toLower guess : guesses) (count + 1) "Try again: "

countUniqueLetters :: [Char] -> Int
countUniqueLetters word = S.size (S.fromList (filter (/= ' ') (map toLower word)))

displayWord :: String -> [Char] -> String
displayWord word guesses = [if toLower c `elem` guesses || c == ' ' then c else '_' | c <- word]

