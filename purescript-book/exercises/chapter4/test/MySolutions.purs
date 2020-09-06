module Test.MySolutions where

import Prelude
import Data.Array (length, filter, (..), nubBy, (:), reverse)
import Control.MonadZero (guard)
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.List as L

-- Note to reader: Add your solutions to this file

isEven :: Int -> Boolean
isEven input = (input `mod` 2) == 0

countEven :: Array Int -> Int
countEven = length <<< filter isEven

squared :: Array Number -> Array Number
squared = map (\x -> x * x)

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter (\x -> x >= 0.0)

infix 8 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite numbers = (\x -> x >= 0.0) <$?> numbers

isPrime :: Int -> Boolean
isPrime number
  | number == 0 || number == 1 = false
  | number == 2 = true
  | otherwise = length (filter (\x -> number `mod` x == 0) (2 .. (number - 1))) == 0

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct arr1 arr2 = do
  i <- arr1
  j <- arr2
  [[i,j]]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ (a*a) + (b*b) == (c*c)
  [[a,b,c]]

factorize :: Int -> Array Int
factorize = reverse <<< removeDuplicates <<< primeFactors

removeDuplicates :: Array Int -> Array Int
removeDuplicates arr = nubBy compare arr

primeFactors :: Int -> Array Int
primeFactors n = do
  let primes = L.filter isPrime (L.range 0 n)
  let firstFactor = firstPrimeFactor n primes
  if isJust firstFactor
    then (fromMaybe 0 firstFactor) : (primeFactors (n / (fromMaybe 0 firstFactor)))
    else []

firstPrimeFactor :: Int -> (L.List Int) -> Maybe Int
firstPrimeFactor n L.Nil = Nothing
firstPrimeFactor n (L.Cons x xs)
  | n `mod` x == 0 = Just x
  | otherwise = firstPrimeFactor n xs
