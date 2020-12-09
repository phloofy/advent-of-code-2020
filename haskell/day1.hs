{-# LANGUAGE FlexibleContexts #-}

module Day1(p1, p2) where

import Control.Monad.ST
import Data.Array.ST
import Data.List
import System.Environment

main :: IO ()
main = undefined

-- part 1
p1 :: FilePath -> IO ()
p1 = (>>= putStrLn) . fmap solve . readFile
  where
    solve :: String -> String
    
    -- -- naive solution
    -- solve = show . foldl (*) 1 . head . go . map read . words
    --   where
    --     go :: [Int] -> [[Int]]
    --     go xs = [(x:y:[]) | (x:ys) <- tails xs, y <- ys, x+y == 2020]

    -- less naive solution (assumes nonnegative input < 2020)
    solve = show . go . map read . words
      where
        go :: [Int] -> Int
        go xs = runST $ (newArray (0,2020) False :: ST s (STArray s Int Bool)) >>= go' xs
        go' (x:xs) arr = do
          b <- readArray arr x
          if b
            then return $ x * (2020 - x)
            else writeArray arr (2020 - x) True >> go' xs arr

-- part 2
p2 :: FilePath -> IO ()
p2 = (>>= putStrLn) . fmap solve . readFile
  where
    solve :: String -> String

    -- -- naive solution
    -- solve = show . foldl (*) 1 . head . go . map read . words
    --   where
    --     go :: [Int] -> [[Int]]
    --     go xs = [(x:y:z:[]) | (x:ys) <- tails xs, (y:zs) <- tails ys, z <- zs, x+y+z == 2020]

    -- less naive solution (assumes nonnegative input < 2020)
    solve = show . go . sort . map read . words
      where
        go :: [Int] -> Int
        go xs = runST $ (newArray (0,2020) 0 :: ST s (STArray s Int Int)) >>= go' xs
          where go' (x:xs') arr = do
                  ans <- twosum (takeWhile (< x) xs) arr (2020 - x)
                  if  ans > 0
                    then return $ x * ans
                    else do val <- readArray arr x
                            writeArray arr x $ val + 1
                            go' xs' arr
                go' _ _ = return $ -1
        twosum (x:xs) arr tgt = do
          a <- readArray arr x
          if x == tgt - x
            then if a > 1
                 then return $ x * x
                 else twosum xs arr tgt
            else if a > 0
                 then do b <- readArray arr (tgt - x)
                         if b > 0
                           then return $ x * (tgt - x)
                           else twosum xs arr tgt
                 else twosum xs arr tgt
        twosum _ _ _ = return $ -1          
        
