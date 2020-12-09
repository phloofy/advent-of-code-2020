module Day2(p1) where

import Data.Char
import System.Environment
import Text.ParserCombinators.ReadP

main :: IO ()
main = do
  args <- getArgs
  case args !! 0 of
    "p1" -> p1 $ args !! 1
    "p2" -> p2 $ args !! 1
    _    -> putStrLn "invalid arguments"

-- part 1
p1 :: FilePath -> IO ()
p1 = (>>= putStrLn) . fmap solve . readFile
  where
    solve :: String -> String
    solve = show . length . filter valid . lines
      where
        valid :: String -> Bool
        valid s = let x = info s
                      (p,q,r) = fst x
                      len = length $ filter (== r) (snd x) in
          len >= p && len <= q
          
-- part 2
p2 :: FilePath -> IO ()
p2 = (>>= putStrLn) . fmap solve . readFile
  where
    solve :: String -> String
    solve = show . length . filter valid . lines
      where
        valid :: String -> Bool
        valid s = let x = info s
                      (p,q,r) = fst x in
          (snd x !! (p-1) == r) /= (snd x !! (q-1) == r)

info :: String -> ((Int,Int,Char),String)
info = head . readP_to_S parser
  where
    parser = do
      x <- read <$> munch1 isDigit :: ReadP Int
      char '-'
      y <- read <$> munch1 isDigit :: ReadP Int
      char ' '
      c <- get
      string ": "
      return (x,y,c)
