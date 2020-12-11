module Day3(p1,p2) where

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args !! 0 of
    "p1" -> p1 $ args !! 1
    "p2" -> p2 $ args !! 2
    _    -> putStrLn "invalid arguments"

interactFile :: (String -> String) -> FilePath -> IO ()
interactFile f = (>>= putStrLn) . fmap f . readFile

-- part 1
p1 :: FilePath -> IO ()
p1 = interactFile $ show . check (3,1) . lines  

-- part 2
p2 :: FilePath -> IO ()
p2 = interactFile $ show . solve . lines
  where
    solve xs = foldl (*) 1 . map (flip check xs) $ [(1,1),(3,1),(5,1),(7,1),(1,2)]

check :: (Int,Int) -> [String] -> Int
check (r,d) xs = go' (r `mod` width) (drop d xs) 0
  where
    go' :: Int -> [String] -> Int -> Int
    go' x xs@(s:_) n = go' ((x+r) `mod` width) (drop d xs) $
      if s !! x == '#' then n+1 else n
    go' _ _ n = n

    -- there is an extra '\r' character
    width = length . takeWhile (or . flip map ['.','#'] . (==)) $ head xs
