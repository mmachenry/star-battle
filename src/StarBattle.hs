module StarBattle where

import FD (FDExpr, FD, runFD, news, labelling, fromInt, (#==), (#<), (#\=))
import Control.Monad (when, zipWithM_, mplus, mzero)
import qualified Data.Set as Set
import qualified Data.Matrix as M
import qualified Data.Vector as V

starBattle regions = filter (regionReject regions) (generate (M.nrows regions))

generate :: Int -> [[(Int, Int)]]
generate size = runFD $ do
  vars1 <- news size (0,size-1)
  vars2 <- news size (0,size-1)
  columns vars1 vars2
  let vars = zip vars1 vars2
  notAdjacent vars
  rows size (vars1++vars2)
  v1 <- labelling vars1
  v2 <- labelling vars2
  return $ zip [0..] v1 ++ zip [0..] v2

columns :: [FDExpr] -> [FDExpr] -> FD ()
columns = zipWithM_ (\a b->a + 1 #< b)

rows :: Int -> [FDExpr] -> FD ()
rows size vars = mapM_ (tally vars 2) [0 .. size-1]
  where tally [] count _ = when (count /= 0) mzero
        tally (x:xs) count n =
          mplus (x #== fromInt n >> tally xs (count-1) n)
                (x #\= fromInt n >> tally xs count n)

notAdjacent [] = return ()
notAdjacent [_] = return ()
notAdjacent ((a1,a2):rest@((b1,b2):_)) = do
  1 #< abs(a1-b1)
  1 #< abs(a1-b2)
  1 #< abs(a2-b1)
  1 #< abs(a2-b2)
  notAdjacent rest

regionReject :: M.Matrix Int -> [(Int,Int)] -> Bool
regionReject regions solution = loop solution (V.replicate (M.nrows regions) 0)
  where loop [] counts = True --V.all (==2) counts
        loop ((x,y):posns) counts =
          let regionId = M.unsafeGet (y+1) (x+1) regions
              current = counts V.! regionId
          in current < 2 && loop posns (counts V.// [(regionId, current + 1)])

