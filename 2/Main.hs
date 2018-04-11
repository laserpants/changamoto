{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative ( (<|>) )
import Control.Monad ( forM_, unless )
import Data.Function ( (&) )
import Data.List ( find )

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = [(x, x') | x' <- xs] ++ pairs xs

distance :: Num a => a -> a -> a
distance w v = abs (w - v)

balanceScale :: (Int, Int) -> [Int] -> Maybe ([Int], [Int])
balanceScale (fst, snd) weights
  | 0 == d = pure mempty               -- Is the scale already balanced?
  | d `elem` weights = balance [] [d]  -- Consider one weight case
  | otherwise = do                     -- Finally, try two weights
      (w, v) <- matchDiffBy (+)
      balance [] [w, v]
    <|> do
      (w, v) <- matchDiffBy distance
      balance [w] [v]
  where
    d = distance fst snd
    balance a b = pure (if fst > snd then (a, b) else (b, a))
    matchDiffBy op = 
      let eq_d (x, y) = d == x `op` y 
       in pairs weights & find eq_d

-- ////////////////////////////////////////////////////////////////////////////

data TestData = Test
  { weights :: [Int] 
  , config  :: (Int, Int) 
  , results :: [Maybe ([Int], [Int])] }

tests :: [TestData]
tests =
  [ Test [1, 2, 6, 7] (5, 9) [Just ([6], [2])]
  , Test [1, 2, 6, 7] (9, 5) [Just ([2], [6])]
  , Test [1, 2, 4, 7] (5, 9) [Just ([4], [])]
  , Test [1, 2, 4, 7] (9, 5) [Just ([], [4])]
  , Test [7, 4, 2, 1] (9, 5) [Just ([], [4])]
  , Test [4, 7, 1, 2] (9, 5) [Just ([], [4])]
  , Test [1, 2, 4, 9] (11, 5) [Just ([], [4, 2]), Just ([], [2, 4])]
  , Test [1, 2, 4, 9] (3, 3) [Just ([], [])]
  , Test [4, 6, 7, 8] (3, 8) [Nothing]
  ]

runTests :: [TestData] -> IO ()
runTests tests = forM_ tests $ \test@Test{..} ->
  let actual = balanceScale config weights in
    unless (actual `elem` results) $
      error ("Test failure: " ++ log test actual)
  where
    log Test{..} result = 
         "\nWeights               : " ++ show weights
      ++ "\nInitial configuration : " ++ show config 
      ++ "\nExpected results      : " ++ show results
      ++ "\nActual result         : " ++ show result

main :: IO ()
main = runTests tests >> print "All tests okay!"
