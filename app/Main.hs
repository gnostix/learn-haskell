module Main (main) where

import Data.List (nub)
import Lib
import Lib (prefixes)
import qualified Lib
import Text.Printf (IsChar (toChar))
import System.IO.Error (isResourceVanishedError)
import System.Environment

-- lala :: Bool -> String
-- lala = do someFunc 'R'
--     | True =  "Alitheia"
--     | otherwise =  "Psemata"

-- data Either a b = Left a | Right b
type OnomaHlikia = Either String  Int
data OnomaHlikia' = OnomaHlikia' {nam :: String,
                                  lastname ::  String,
                                  ag :: Int}
  deriving Show

add :: Integer -> Integer -> Integer -- function declaration
add x y = x + y

ele :: (Eq a) => a -> [a] -> Bool
ele _ [] = False
ele e (x : xs) = (e == x) || (ele e xs)

add1 :: Int -> Int
add1 x = x + 1

add2 :: Int -> Int -> Int
add2 = (\x -> (\y -> x + y))

-- f :: Ord a => [a] -> [a]
-- f xs = reverse . sort xs

main :: IO ()
main = do
  -- putStrLn "Please enter name"
  -- name <- getLine
  -- let uname = map toUpper name
  -- putStrLn name

  -- putStrLn "Hello, Haskell!"
  -- putStrLn isZero 8
  -- print (add 2 5)
  -- print (ele 5 [1,2,3])
  -- someFunc'
  -- someFunc 'L'
  -- someFunc 'R'
  -- print (Lib.isAsc' [6,3])
  -- print (isAsc' [1, 1, 6, 3, 4, 4, 5])
  -- print (Lib.isAsc' [1,1,3,4,4,5])
  -- print (Lib.isAsc' [1])
  -- print (Lib.isAsc' [])
  -- let li = [(1,2),(2,3),(3,2),(4,3),(4,5)]
  -- print (Lib.hasPath li 1 4)

  -- print (myFunc add1 3 )
  print (myFunc (\x -> x * x) 3)
  -- print (map (\a -> a * 2) [1,2,3,4,5])
  -- print (map (uncurry (+)) [(1,1),(2,2),(3,3)])
  -- print (filter (\a -> a > 2) [1,2,3,4,5])
  -- -- print (map2D (\x -> x + 2), [[1,2,3]])
  -- print (foldr (\x acc -> if rem x 2 == 0 then acc * x else acc) 1  [1,2,3])
  print
    (foldr (\x acc -> acc + 1) 0 [1, 2, 3, 4, 5, 6])
  -- print( hasPath [(1,2),(3,4)])
  -- print (monadd (Just 5) (Just 6))
  let tr = cut 2 inv_tup_tree 
  let tr' = cut 5 inv_tup_tree 
  print (tr)
  -- let tr' = insert' 2 tr 
  print (inorder tr')

  -- either :: (a -> c) -> (b -> c) -> Either a b -> c
  -- let f = either (\l -> "Number") (\r -> r)
  -- print ( getEnvironement)  
  mapM_ putStrLn =<< map fst `fmap` getEnvironment

  -- lala :: OnomaHlikia'
  -- lala ["Alex" "Pappas" 45]
  -- print (lala.nam)  
  -- print
  --   (foldr (\x acc -> acc + 1) 0 [1, 2, 3, 4, 5, 6])
  

  -- print(foldtrie (:) [] Node 'c' [Node 'a' [Leaf 'r', Leaf 't'], Node 'o' [Node 'o' [Leaf 'l']]])
data Person = Person { name :: String,
                       age :: Int }
data Temp = Kelvin Float | Celsius Float | Fahrenheit Float deriving Show
  

