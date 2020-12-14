{-# LANGUAGE TupleSections #-}

import Advent

import Control.Applicative ((<|>))
import Data.Array
import Data.Maybe
import qualified Data.Set as Set
import Text.ParserCombinators.ReadP

-- blackbird
infixr 7 ...
(...) = (.) . (.)

p1 :: FilePath -> IO ()
p1 = interactFile $ show . solve . (\xs -> listArray (0,length xs) xs) . lines
  where
    solve :: Array Int String -> Int
    solve arr = go Set.empty 0 0
      where
        go :: Set.Set Int -> Int -> Int -> Int
        go s i x
          | Set.member i s = x
          | otherwise = let (p,q) = delta . instruction $ arr ! i in
              go (Set.insert i s) (i+p) (x+q)
        delta (ins,v) = case ins of
          "nop" -> (1,0)
          "acc" -> (1,v)
          "jmp" -> (v,0)

p2 :: FilePath -> IO ()
p2 = interactFile $ show . solve . lines
  where
    solve :: [String] -> Int
    solve xs = fromJust $ go Set.empty 0 0
      where
        go :: Set.Set Int -> Int -> Int -> Maybe Int
        go s i x
          | Set.member i s = Nothing
          | i == len       = error "unexpected success"
          | otherwise      = let s' = Set.insert i s
                                 (ins,v) = arr ! i in
              case ins of
                "nop" -> (go s' (i+1) x) <|> (go' s' (i+v) x)
                "acc" -> go s' (i+1) (x+v)
                "jmp" -> (go' s' (i+1) x) <|> (go s' (i+v) x)
                
        go' :: Set.Set Int -> Int -> Int -> Maybe Int
        go' s i x
          | Set.member i s = Nothing
          | i == len       = Just x
          | otherwise      = let s' = Set.insert i s
                                 (ins,v) = arr ! i in
              case ins of
                "nop" -> go' s' (i+1) x
                "acc" -> go' s' (i+1) (x+v)
                "jmp" -> go' s' (i+v) x

        arr = listArray (0,len) $ map instruction xs
        len = length xs

instruction :: String -> (String,Int)
instruction = fst . head ... readP_to_S $ do
  ins <- choice [string "nop",string "acc",string "jmp"]
  skipSpaces >> optional (char '+') >> (ins,) <$> read <$> (many get <* eof)
