module StarBattle where

import FD (FDExpr, FD, runFD, news, labelling, (#==), (#<), (#\=))
import Control.Applicative ((<|>))
import Control.Monad (filterM, zipWithM, mplus)
import qualified Data.Set as Set
import qualified Data.Matrix as M
import qualified Data.Vector as V

starBattle regions = filter (regionReject regions) generate

generate :: [[(Int, Int)]]
generate = runFD $ do
  vars1 <- news 10 (0,9)
  vars2 <- news 10 (0,9)
  let vars = zip vars1 vars2
  columns vars
  notAdjacent vars
  rows (vars1++vars2)
  v1 <- labelling vars1
  v2 <- labelling vars2
  return $ zip [0..] v1 ++ zip [0..] v2

columns = mapM (\(a,b)->a + 1 #< b)

rows vars = mapM (tally vars 2) [0..9]
  where tally [] count _ = count #== 0
        tally (x:xs) count n =
          mplus (x #== n >> tally xs (count-1) n)
                (x #\= n >> tally xs count n)

notAdjacent [] = return ()
notAdjacent [_] = return ()
notAdjacent ((a1,a2):rest@((b1,b2):_)) = do
  1 #< abs(a1-b1)
  1 #< abs(a1-b2)
  1 #< abs(a2-b1)
  1 #< abs(a2-b2)
  notAdjacent rest

-- {-
regionReject :: M.Matrix Int -> [(Int,Int)] -> Bool
regionReject regions solution = loop solution (V.replicate 10 0)
  where loop [] counts = True --V.all (==2) counts
        loop ((x,y):posns) counts =
          let regionId = M.unsafeGet (y+1) (x+1) regions
              current = counts V.! regionId
          in if current < 2
             then loop posns (counts V.// [(regionId, current + 1)])
             else False
-- -}

 {- 
regionReject :: [Set.Set (Int,Int)] -> [(Int,Int)] -> Bool
regionReject regions ns = all (region (Set.fromList ns)) regions

region :: Set.Set (Int,Int) -> Set.Set (Int,Int) -> Bool
region a b = Set.size (Set.intersection a b) == 2
-- -}

