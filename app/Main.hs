module Main where

import StarBattle

main :: IO ()
main = mapM_ print $ take 1 starBattle
