module Lib
  ( someFunc,
    someFunc',
    isZero,
    fac,
    asc,
    nub,
    isAsc,
    isAsc',
    hasPath,
    myFunc,
    map2D,
    rev,
    prefixes,
    sum2,
    sum3,
    monadd,
    monadd',
  )
where

import Control.Concurrent (yield)
import Data.Tuple
import Data.List (sort)
import Data.List.NonEmpty (xor, break)

someFunc :: Char -> IO ()
someFunc str
  | str == 'R' = putStrLn "True"
  | otherwise = putStrLn "False"

someFunc' :: IO ()
someFunc' = putStrLn "someFunc'"

fac :: (Eq p, Num p) => p -> p
fac 0 = 1
fac n = n * fac (n - 1)

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' e (x : xs) = (e == x) || (elem' e xs)

isZero :: Int -> Bool
isZero 0 = True
isZero _ = False

asc :: Int -> Int -> [Int]
asc n m
  | m < n = []
  | m == n = [m]
  | m > n = n : asc (n + 1) m

nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x : xs)
  | x `elem'` xs = nub xs
  | otherwise = x : nub xs

isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [x] = True
isAsc (x : y : xs)
  | x <= y = isAsc (y : xs)
  | otherwise = False

isAsc' :: [Int] -> Bool
isAsc' [] = True
isAsc' [x] = True
isAsc' (x : y : xs) = x <= y && isAsc (y : xs)

hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath [] s e = s == e
hasPath xs s e
  | s == e = True
  | otherwise =
      let xs' = [(n, m) | (n, m) <- xs, n /= s]
       in or [hasPath xs' m e | (n, m) <- xs, n == s]

myFunc :: (a -> b) -> a -> b
myFunc f x = f x

map2D :: (a -> b) -> [[a]] -> [[b]]
map2D = map . map

-- lagrange :: [(Float, Float)] -> Float -> Float
-- lagrange xs x = foldl (\acc (xj,y) -> acc + (y * l xj)) 0
-- xs
--   where
--     l xj = foldl (
--       \acc (xk, _) ->
--         if xj==xk then
--           acc
--           else
--             acc * ((x-xk) / (xj-xk))
--     ) 1 xs

data Trie a = Leaf a | Node a [Trie a]

foldtrie :: (b -> a -> b) -> b -> Trie a -> b
foldtrie f acc (Leaf x) = f acc x
foldtrie f acc (Node x xs) = foldl f' (f acc x) xs
  where
    f' acc t = foldtrie f acc t

    tr =
      Node
        'c'
        [ Node
            'a'
            [Leaf 'r', Leaf 't'],
          Node
            'o'
            [ Node
                'o'
                [Leaf 'l']
            ]
        ]

-- foldtrie =  (\acc x -> if(x == Node) then acc : foldtrie x else acc : x)

rev :: [a] -> [a]
rev = foldl (\acc xs -> xs : acc) []

prefixes :: [a] -> [[a]]
prefixes = foldr (\x acc  -> [x] : (map ((:) x) acc)) []

sum2 :: Num a => [a] -> a
sum2 [] = 0
sum2 (x:xs) = x + sum2 xs 

sum3 :: Num a => [a] -> a
sum3 = foldr (\acc x -> acc + x) 0

monadd :: (Monad m, Num b) => m b -> m b -> m b
monadd mx my =  mx >>= (\x -> my >>= (\y -> return $ x + y))

monadd' :: (Monad m, Num b) => m b -> m b -> m b
monadd' mx my = do
  x <- mx
  y <- my
  return $ x + y