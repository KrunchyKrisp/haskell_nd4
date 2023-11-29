module Main where

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


--3


--4


--5


--6


--7