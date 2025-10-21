-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW0.hs should successfully compile.
--
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW1 where

-- These import statement ensures you aren't using any "advanced" functions and types, e.g., lists.
import Prelude (Bool (..), Eq (..), Int, Integer, Num (..), Ord (..), div, error, even, flip, id, mod, not, otherwise, undefined, ($), (&&), (.), (||))

------------------------------------------------
-- DO NOT MODIFY ANYTHING ABOVE THIS LINE !!! --
------------------------------------------------

-- ********* --
-- Section 1
-- ********* --

const :: a -> b -> a
const x _ = x
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) f g a= g (f a)
curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f(a, b, c) 
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a,b,c) = f a b c
rotate :: (a -> b -> c -> d) -> c -> a -> b -> d
rotate f c a b = f a b c
lotate :: (a -> b -> c -> d) -> b -> c -> a -> d
lotate f b c a = f a b c 
-- Generalizations of (.)
(.:) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.:) f g a b c = f (g a b c)
(.:.) :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
(.:.) f g a b c d = f (g a b c d)
(.::) :: (f -> g) -> (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> g
(.::) f g a b c d e = f (g a b c d e)
(.::.) :: (g -> h) -> (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> h
(.::.) h g a b c d e f = h (g a b c d e f)
-- How can we ever implement such a function!?
impossible :: a -> b
impossible _ = undefined

-- ********* --
-- Section 2
-- ********* --
countDigits :: Integer -> Integer
countDigits x
    | abs x < 10 = 1
    | otherwise = 1 + countDigits(x `div` 10)
toBinary :: Integer -> Integer
toBinary 0 = 0
toBinary n
    | n < 0 = (-1) * toBinary (-n)
    | otherwise =   (n`mod`2) + 10 * toBinary (n `div`2)
fromBinary :: Integer -> Integer
fromBinary 0 = 0
fromBinary n 
  | n < 0     = (-1) * fromBinary (-n)
  | otherwise = (n `mod` 10) + 2 * fromBinary (n `div` 10)
isAbundant :: Integer -> Bool
isAbundant n
    | n < 2     = False
    | (sumDivisors n 1) > n  = True
    | otherwise = False
sumDivisors :: Integer -> Integer -> Integer
sumDivisors n d
    | d > (n `div` 2)     = 0
    | n `mod` d == 0      = d  + sumDivisors n (d + 1)    
    | otherwise           = sumDivisors n (d + 1)


rotateDigits :: Integer -> Integer
rotateDigits n 
    | n < 0 = - (rotateDigitsToRight (-n))
    | otherwise = rotateDigitsToLeft n
rotateDigitsToLeft :: Integer -> Integer
rotateDigitsToLeft n = (n `mod` powerOf10) * 10 + firstDigit
  where
    powerOf10 =  tenToThePower (countDigits n - 1) 0
    firstDigit = n `div` powerOf10 

rotateDigitsToRight :: Integer -> Integer
rotateDigitsToRight n = lastDigit * powerOf10 + (n `div` 10)
  where
    powerOf10 =  tenToThePower (countDigits n - 1) 0
    lastDigit = n `mod` 10 
tenToThePower :: Integer -> Integer -> Integer
tenToThePower exp currentExp
    | exp == currentExp = 1
    | otherwise = 10 * tenToThePower exp (currentExp + 1)



-- ********* --
-- Section 3
-- ********* --
type Generator a = (a -> a, a -> Bool, a)

positives :: Generator Integer
positives = ((+ 1) , const True , 0)

nullGen :: Generator a -> Bool
nullGen (_, t, s) = not (t s)

lastGen :: Generator a -> a
lastGen (f, t, s) = if t s then lastGen (f, t, f s) else s

lengthGen :: Generator a -> Int
lengthGen (f, t, s) = if t s then 1 + lengthGen (f, t, f s) else 0

sumGen :: Generator Integer -> Integer
sumGen (f,g,x) 
    | not (g x) = 0
    | otherwise = sumGen (f,g, f x) + f x

type Predicate a = a -> Bool
positivesUpTo10 :: Generator Integer
positivesUpTo10 = ((+ 1) , (< 11) , 0)
anyGen :: Predicate a -> Generator a -> Bool
anyGen p (f,g,x)
    | g x, p (f x) = True
    | g x, not (p (f x)) = anyGen p (f,g, f x)
    | otherwise = False
allGen :: Predicate a -> Generator a -> Bool
allGen p (f,g,x)
    | g x, p (f x) = allGen p (f,g, f x)
    | g x, not (p (f x)) = False
    | otherwise = True
noneGen :: Predicate a -> Generator a -> Bool
noneGen t gen = not (anyGen t gen)
countGen :: Predicate a -> Generator a -> Int
countGen p (f,g,x)
    | g x, p (f x) = countGen p (f,g, f x) + 1
    | g x, not (p (f x)) = countGen p (f,g, f x)
    | otherwise = 0

-- ********* --
-- Section 4
-- ********* --
isPrime :: Integer -> Bool
isPrime n
    | n < 2     = False  
    | otherwise = checkDivisors n 2 
checkDivisors :: Integer -> Integer -> Bool
checkDivisors n d
    | d * d > n        = True   
    | n `mod` d == 0   = False  
    | otherwise        = checkDivisors n (d + 1) 
isSemiprime :: Integer -> Bool
checkSemiprime :: Integer -> Integer ->  Bool
checkSemiprime n d
    | d * d >= n                                        =  False
    | n `mod` d == 0, isPrime d && isPrime(n `div` d)   = True  
    | otherwise                                         = checkSemiprime n (d + 1) 
isSemiprime n = checkSemiprime n 2

goldbachPair :: Integer -> (Integer, Integer)
goldbachPair n = goldbachPairHelper n 2
goldbachPairHelper :: Integer -> Integer -> (Integer, Integer)
goldbachPairHelper n d
    | isPrime d && isPrime (n - d) = (d, n - d)
    | otherwise = goldbachPairHelper n (d + 1)
goldbachPair' :: Integer -> (Integer, Integer)
goldbachPair' n = goldbachPairHelper n (n `div` 2)

-- ***** --
-- Bonus
-- ***** --
isCircularPrime :: Integer -> Bool
isCircularPrimeHelper :: Integer -> Integer -> Bool
isCircularPrimeHelper n d
    | n == d   = True       -- made a full circle
    | isPrime d, countDigits n /= countDigits d = isCircularPrimeHelper n (rotateDigits d * 10) -- this case is when there was a leading zero that was removed and we need to add back
    | isPrime d = isCircularPrimeHelper n (rotateDigits d)
    | otherwise = False
-- If you choose the implement this function, replace this with the actual implementation
isCircularPrime n = isCircularPrimeHelper n (rotateDigits n)

