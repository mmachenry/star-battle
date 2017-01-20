module Main where

import System.Environment (getArgs)
import StarBattle (starBattle)
import ParseMatrix (parseMatrix)
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  [fileName] <- getArgs
  text <- BL.readFile fileName
  let regions = parseMatrix text
  mapM_ print $ take 1 $ starBattle regions

