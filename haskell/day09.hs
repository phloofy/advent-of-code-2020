import Advent

import Data.Foldable (toList)
import Data.Sequence as Seq hiding (take,drop)
import qualified Data.Set as Set

p1 :: FilePath -> IO ()
p1 = interactFile $ show . solveP1 . map read . words

p2 :: FilePath -> IO ()
p2 = interactFile $ show . solve . map read . words
  where
    solve :: [Int] -> Int
    solve xs = go xs xs 0 Seq.empty
      where
        go :: [Int] -> [Int] -> Int -> Seq Int -> Int
        go xs@(x:xs') ys@(y:ys') p s
          | p < tgt = go xs ys' (p+y) (s|>y)
          | p > tgt = go xs' ys (p-x) $ case s of
              (_:<|s) -> s
              otherwise -> error "invalid input"
          | otherwise = let (p,q) = findMinMax $ toList s in p+q
        go _ _ _ _ = error "invalid input"
          
        tgt = solveP1 xs

solveP1 :: [Int] -> Int
solveP1 xs = go (Seq.fromList $ take 25 xs) $ drop 25 xs
  where
    go :: Seq Int -> [Int] -> Int
    go s@(_:<|s') (x:xs)
      | hasTwosum s x = go (s'|>x) xs
      | otherwise     = x
    go _ _ = error "invalid input"

hasTwosum :: Seq Int -> Int -> Bool
hasTwosum s t = go Set.empty (toList s)
  where
    go :: Set.Set Int -> [Int] -> Bool
    go s (x:xs)
      | Set.member x s = True
      | otherwise = go (Set.insert (t-x) s) xs
    go _ _ = False

findMinMax :: Ord a => [a] -> (a,a)
findMinMax []        = error "empty list"
findMinMax (x:[])    = (x,x)
findMinMax (x:x':xs) = go xs (min x x',max x x')
  where
    go (x:xs) (p,q) = go xs $ case compare x p of
      LT -> (x,q)
      EQ -> (p,q)
      GT -> case compare x q of
        LT -> (p,q)
        EQ -> (p,q)
        GT -> (p,x)
    go _ (p,q) = (p,q)
