import Advent

import Control.Monad.State
import Data.Char
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Text.ParserCombinators.ReadP hiding (get)
import qualified Text.ParserCombinators.ReadP as ReadP (get)

type Color = String
type Rule = (Color,Map.Map Color Int)

p1 :: FilePath -> IO ()
p1 = interactFile $ show . solve . rules . map (filter (/= '\r')) . lines
  where
    solve :: [Rule] -> Int
    solve xs = fst . runState (go xs) $ Set.singleton "shiny gold"
    
    go :: [Rule] -> State (Set.Set Color) Int
    go xs = do
      k <- Set.size <$> get
      xs' <- fmap (map fromJust . filter isJust) . forM xs $ \(x,m) -> do
        s <- get
        let i = Set.intersection s (Map.keysSet m)
        if Set.null i
          then return $ Just (x,m)
          else put (Set.insert x s) >> return Nothing
      k' <- Set.size <$> get
      if (k == k')
        then return $ k-1
        else go xs'

p2 :: FilePath -> IO ()
p2 = interactFile $ show . solve . rules . map (filter (/= '\r')) . lines
  where
    solve :: [Rule] -> Int
    solve xs = fst $ runState (go "shiny gold") Map.empty    
      where
        go :: Color -> State (Map.Map Color Int) Int
        go c = let xs = Map.toList . fromJust $ Map.lookup c rm in
          fmap sum . forM xs $ \(x,p) -> do
          m <- get
          case Map.lookup x m of
            (Just q) -> return $ p*(q+1)
            Nothing -> do
              q <- go x
              put $ Map.insert x q m
              return $ p*(q+1)
        rm = Map.fromList xs    
             
rules :: [String] -> [Rule]
rules = map $ fst . head . readP_to_S parser
  where
    parser :: ReadP Rule
    parser = do
      k <- manyTill ReadP.get (string " bags contain")
      m <- choice [fmap Map.fromList . many $ do
                      n <- read <$> (skipSpaces >> munch1 isDigit) :: ReadP Int
                      c <- skipSpaces >> manyTill ReadP.get (string " bag") <*
                        (optional (char 's') >> optional (char ','))
                      return (c,n),
                   string " no other bags" >> return Map.empty]
      char '.' >> eof >> return (k,m)
      
        
      
