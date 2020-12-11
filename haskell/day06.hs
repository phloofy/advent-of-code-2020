import Advent

import Data.Char
import qualified Data.Set as Set

p1 :: FilePath -> IO ()
p1 = interactFile $ show . sum . map go . wordsBy (== "\r") . lines
  where
    go = Set.size . mconcat . map (Set.fromList . filter isAsciiLower)

p2 :: FilePath -> IO ()
p2 = interactFile $ show . sum . map go . wordsBy (== "\r") . lines
  where
    go = Set.size . foldr1 Set.intersection . map (Set.fromList . filter isAsciiLower)
