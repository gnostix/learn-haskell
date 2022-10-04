module Main (main) where

import Lib
import Data.List (nub)
import qualified Lib
import Text.Printf (IsChar(toChar))
import Lib (prefixes)


-- lala :: Bool -> String
-- lala = do someFunc 'R'
--     | True =  "Alitheia"
--     | otherwise =  "Psemata"

add :: Integer -> Integer -> Integer   --function declaration 
add x y =  x + y


ele :: (Eq a) => a -> [a] -> Bool
ele _ [] = False
ele e (x:xs) = (e == x) || (ele e xs)

add1 :: Int -> Int
add1 x = x + 1

add2 :: Int -> Int -> Int
add2 = (\x -> (\y -> x + y))

main :: IO ()
main = do

  putStrLn "Hello, Haskell!"
  -- putStrLn isZero 8
  -- print (add 2 5)  
  -- print (ele 5 [1,2,3])
  -- someFunc'
  -- someFunc 'L'
  -- someFunc 'R'
  -- print (Lib.isAsc' [6,3])
  -- print (Lib.isAsc' [1,1,6,3,4,4,5])
  -- print (Lib.isAsc' [1,1,3,4,4,5])
  -- print (Lib.isAsc' [1])
  -- print (Lib.isAsc' [])
  -- let li = [(1,2),(2,3),(3,2),(4,3),(4,5)]
  -- print (Lib.hasPath li 1 4)

  print (myFunc add1 3 )
  print (myFunc (\x -> x * x) 3 )
  print (map (\a -> a * 2) [1,2,3,4,5])
  print (map (uncurry (+)) [(1,1),(2,2),(3,3)])
  print (filter (\a -> a > 2) [1,2,3,4,5])
  -- print (map2D (\x -> x + 2), [[1,2,3]])
  print (foldr (\x acc -> if rem x 2 == 0 then acc * x else acc) 1  [1,2,3])
  print( foldr (\x acc -> acc +1) 0 [1,2,3,4,5,6])
  print (prefixes [1,2,3,4,5])