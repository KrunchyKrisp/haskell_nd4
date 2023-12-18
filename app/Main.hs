{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fromMaybe" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}
module Main where
import Prelude hiding ((<*>))

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"

--1
data GTree a = Leaf a | Gnode [GTree a]

depth :: GTree a -> Int
depth (Leaf _) = 1
depth (Gnode []) = 1
depth (Gnode xs) = 1 + maximum (map depth xs)

occurs :: Eq a => GTree a -> a -> Bool
occurs (Leaf a) x = a == x
occurs (Gnode []) _ = False
occurs (Gnode xs) x = any (`occurs` x) xs

mapGTree :: (a -> b) -> GTree a -> GTree b
mapGTree f (Leaf a)  = Leaf (f a)
mapGTree _ (Gnode []) = Gnode []
mapGTree f (Gnode xs)  = Gnode (map (mapGTree f) xs)

--2
data Expr a = Lit a | EVar Var | Op (Ops a) [Expr a]
type Ops a = [a] -> a
type Var = Char

type Valuation a = [(Var, a)]

eval :: Valuation a -> Expr a -> a
eval _ (Lit x) = x
eval val (EVar var) = maybe (error "var doesn't exist") id (lookup var val)
eval val (Op f exprs) = f (map (eval val) exprs)


--3
type RegExp = String -> Bool

epsilon :: RegExp
epsilon = (=="")

char :: Char -> RegExp
char ch = (==[ch])

(|||) :: RegExp -> RegExp -> RegExp
e1 ||| e2 = \x -> e1 x || e2 x

splits :: [a] -> [([a], [a])]
splits xs = zip left right
    where
        split = map (: []) xs
        left = scanl (++) [] split
        right = scanr (++) [] split

(<*>) :: RegExp -> RegExp -> RegExp
e1 <*> e2 = \x -> or [e1 y && e2 z | (y,z) <- splits x]

star :: RegExp -> RegExp
star p = epsilon ||| (p <*> star p)

option, plus :: RegExp -> RegExp
option p = epsilon ||| p
plus p = p <*> star p

--4
data NumList a = Nlist [a]

average :: (Fractional a) => [a] -> a
average [] = 0
average xs = sum xs / fromIntegral (length xs)

instance (Eq a, Fractional a) => Eq (NumList a) where
    (==) :: (Eq a, Fractional a) => NumList a -> NumList a -> Bool
    (==) (Nlist a) (Nlist b) = average a == average b

instance (Ord a, Fractional a) => Ord (NumList a) where
    compare :: (Ord a, Fractional a) => NumList a -> NumList a -> Ordering
    compare (Nlist a) (Nlist b) = compare (average a) (average b)

--5
data Result a = OK a | Error String

composeResult :: (a -> Result b) -> (b -> Result c) -> (a -> Result c)
composeResult f1 f2 x
    | OK y <- f1 x = f2 y
    | Error error <- f1 x = Error error

--6
primes :: [Integer]
primes = sieve [2 ..]
sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x > 0]

goldbach :: Integer -> Bool
goldbach n = all (`elem` sums) evenNums
    where 
        evenNums = [x | x <- [4..n], even x]
        sums = [x+y | x <- takeWhile (< n) primes, y <- takeWhile (< n) primes, (x+y) <= n && even (x+y)]

--7
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x y) = x : streamToList y

streamIterate :: (a -> a) -> a -> Stream a
streamIterate f a = Cons a (streamIterate f (f a))

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons a as) (Cons b bs) = Cons a (Cons b (streamInterleave as bs))