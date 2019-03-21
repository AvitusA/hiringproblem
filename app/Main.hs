module Main where

import System.Environment
import System.Random
import System.Random.Shuffle
import Data.List.Split

main :: IO ()
main = do
  g <- newStdGen
  args <- getArgs
  if length args /= 2 then
    print "Pass <n candidates> and <n samples> as cmd line args"
  else
    let
      -- Number of candidates to interview
      n = read (args !! 0) :: Integer
      -- Number of samples for simulation
      s = read (args !! 1) :: Integer
      -- We will make one giant permutation and use it for all samples
      l = [1..(n * s)]
      seqs = chunksOf (fromInteger n) (shuffle' l (fromInteger (n * s)) g)
    in
      -- Count the proportion of samples where exactly 2 agents were hired
      print ((fromIntegral (length (filter (== 2) (map hire seqs))) /
              fromIntegral s) :: Double)

hire :: [Integer] -> Integer
hire candidates = hire' candidates 0 0
  where
    hire' :: [Integer] -> Integer -> Integer -> Integer
    -- hire' candidates nHired best
    hire' [] h _b = h
    hire' (c:cs) h b =
      if c > b then
        hire' cs (h + 1) c
      else
        hire' cs h b
