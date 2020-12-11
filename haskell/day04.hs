module Day4(p1,p2) where

import Data.Char
import Data.Maybe
import qualified Data.Set as Set
import Text.ParserCombinators.ReadP

main :: IO ()
main = undefined

interactFile :: (String -> String) -> FilePath -> IO ()
interactFile f = (>>= putStrLn) . fmap f . readFile

p1 :: FilePath -> IO ()
p1 = interactFile $ show . solve . map (>>= words) . wordsBy (== "\r") . lines
  where
    solve :: [[String]] -> Int
    solve = length . filter (Set.isSubsetOf required . Set.fromList . map key)

p2 :: FilePath -> IO ()
p2 = interactFile $ show . solve . map (>>= words) . wordsBy (== "\r") . lines
  where
    solve :: [[String]] -> Int
    solve = length . filter valid
    valid = Set.isSubsetOf required . Set.fromList . map (maybe "" id . validKey)

wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy f xs = go (reverse xs) [] []
  where
    go [] [] zs = zs
    go [] ys zs = (ys:zs)
    go (x:xs) [] zs
      | f x       = go xs [] zs
      | otherwise = go xs (x:[]) zs      
    go (x:xs) ys zs
      | f x       = go xs [] (ys:zs)
      | otherwise = go xs (x:ys) zs

required :: Set.Set String
required = Set.fromList ["byr","iyr","eyr","hgt","hcl","ecl","pid"]

key :: String -> String
key = fst . head . readP_to_S (manyTill get (char ':'))

validKey :: String -> Maybe String
validKey = fmap fst . listToMaybe . readP_to_S (choice [byr,iyr,eyr,hgt,hcl,ecl,pid])
  where
    digitInt :: Int -> ReadP Int
    digitInt = fmap read . flip count (satisfy isDigit)

    inRange :: Ord a => (a,a) -> a -> Bool
    inRange (p,q) x = x >= p && x <= q
    
    byr = do
      x <- string "byr:" >> (digitInt 4 <* eof)
      if inRange (1920,2002) x then return "byr" else pfail
    iyr = do
      x <- string "iyr:" >> (digitInt 4 <* eof)
      if inRange (2010,2020) x then return "iyr" else pfail
    eyr = do
      x <- string "eyr:" >> (digitInt 4 <* eof)
      if inRange (2020,2030) x then return "eyr" else pfail
    hgt = string "hgt:" >> read <$> munch1 isDigit >>= \x ->
      choice [string "cm" >> if inRange (150,193) x then return "hgt" else pfail,
              string "in" >> if inRange (59,76)   x then return "hgt" else pfail]
    hcl = string "hcl:#" >> count 6 (satisfy isHexDigit) >> eof >> return "hcl"
    ecl = string "ecl:" >>
      choice (map string ["amb","blu","brn","gry","grn","hzl","oth"]) >> return "ecl"
    pid = string "pid:" >> count 9 (satisfy isDigit) >> eof >> return "pid"
