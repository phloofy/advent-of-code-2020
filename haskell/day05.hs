module Day5(p1,p2) where

import Data.Bits
import Data.List

main :: IO ()
main = undefined

interactFile :: (String -> String) -> FilePath -> IO ()
interactFile f = (>>= putStrLn) . fmap f . readFile

p1 :: FilePath -> IO ()
p1 = interactFile $ show . go 10 0 . map (map convert) . words
  where
    go :: Int -> Int -> [[Bool]] -> Int
    go 0 ans _ = ans
    go r ans xs = let xs' = filter head xs in
      case xs' of
        []        -> go (r-1) ans (map tail xs)
        otherwise -> go (r-1) (ans .|. (shiftL 1 (r-1))) (map tail xs')

p2 :: FilePath -> IO ()
p2 = interactFile $ show . go . sort . map (readBinary . map convert) . words
  where
    go :: [Int] -> Int
    go (x:xs@(x':_))
      | x'-x == 2 = x'-1
      | otherwise = go xs
    go _ = error "invalid input"

convert :: Char -> Bool
convert 'B' = True
convert 'R' = True
convert _   = False

readBinary :: [Bool] -> Int
readBinary = go 1 0 . reverse
  where
    go i ans (True :xs) = go (shiftL i 1) (ans .|. i) xs
    go i ans (False:xs) = go (shiftL i 1) ans xs
    go _ ans _ = ans
