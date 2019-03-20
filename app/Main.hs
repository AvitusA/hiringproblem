module Main where

import System.Environment
import System.Random
import System.Random.Shuffle
import Data.List.Split

main :: IO ()
main = do
  g <- newStdGen
  args <- getArgs
  let
    n = read (args !! 0) :: Integer
    s = read (args !! 1) :: Integer
    l = [1..(n * s)]
    seqs = chunksOf (fromInteger n) (shuffle' l (fromInteger (n * s)) g)
    in
    print ((fromIntegral (length (filter (== 2) (map hire seqs)))) / (fromIntegral s))

hire :: [Integer] -> Integer
hire candidates = hire' candidates 0 0
  where
    hire' :: [Integer] -> Integer -> Integer -> Integer
    hire' [] h _b = h
    hire' (c:cs) h b =
      case c > b of
        True -> hire' cs (h + 1) c
        False -> hire' cs h b
