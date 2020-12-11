module Advent(interactFile,wordsBy) where

-- misc utility functions

-- interact with a file
interactFile :: (String -> String) -> FilePath -> IO ()
interactFile f = (>>= putStrLn) . fmap f . readFile

-- split a list on a predicate
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

