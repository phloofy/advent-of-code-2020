import Advent

import Control.Applicative (liftA2,(<|>))
import Data.List (sort)
import Data.Maybe (listToMaybe,fromJust)

p1 :: FilePath -> IO ()
p1 = interactFile $ show . solve . sort . map read . words
  where
    solve :: [Int] -> Int
    solve xs = uncurry (*) . foldr go (0,1) $ zipWith (-) xs (0:xs)

    go :: Int -> (Int,Int) -> (Int,Int)
    go 1 (p,q) = (p+1,q)
    go 3 (p,q) = (p,q+1)
    go _ t     = t
      
p2 :: FilePath -> IO ()
p2 = interactFile $ show . solve . sort . map read . words
  where
    solve :: [Int] -> Int
    solve = flip go [(0,1)]

    go :: [Int] -> [(Int,Int)] -> Int
    go (x:xs) ys = let p = listToMaybe ys >>= go' x
                       q = listToMaybe (drop 1 ys) >>= go' x
                       r = listToMaybe (drop 2 ys) >>= go' x
                       s = fromJust $ go'' [p,q,r] <|> go'' [p,q] <|> go'' [p] in
      go xs ((x,s):ys)
    go _ ((n,c):ys) = c

    go' x (n,c) = if x-n > 3 then Nothing else Just c
    go'' = foldr1 $ liftA2 (+)
