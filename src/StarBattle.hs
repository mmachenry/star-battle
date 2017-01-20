module StarBattle where

import FD (FDExpr, FD, runFD, news, labelling, (#==), (#<), (#\=))
import Control.Applicative ((<|>))
import Control.Monad (filterM, zipWithM, mplus)
import qualified Data.Set as Set
import qualified Data.Matrix as Matrix
import qualified Data.Vector as Vector

-- The correct answer to this puzzle is
-- [(0,1),(1,6),(2,2),(3,0),(4,3),(5,0),(6,2),(7,7),(8,3),(9,1),(0,4),(1,8),(2,4),(3,8),(4,6),(5,9),(6,5),(7,9),(8,5),(9,7)]

{-
regions = map Set.fromList [
  [(0,0),(1,0),(0,1),(3,1),(0,2),(1,2),(2,2),(3,2)], -- beige
  [(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(1,1),(2,1)], -- darksea
  [(8,0),(9,0),(9,1),(6,2),(7,2),(8,2),(9,2)], -- purple
  [(4,1),(5,1),(6,1),(7,1),(8,1),(4,2),(0,3),(1,3),(2,3),(3,3),
   (4,3),(0,4)], -- green
  [(5,2),(5,3),(6,3),(6,4),(6,5),(6,6),(6,7),(7,7)], -- beige
  [(7,3),(8,3),(9,3),(9,4),(8,5),(9,5)], -- light green
  [(1,4),(2,4),(3,4),(4,4),(5,4),(0,5),(1,5),(0,6),(0,7),
   (2,7),(3,7),(0,8),(3,8),(0,9),(1,9),(2,9),(3,9)], --lightblue
  [(7,4),(8,4),(7,5),(7,6),(8,6),(9,6),(9,7),(9,8), (7,9),(8,9),(9,9)], -- red
  [(2,5),(3,5),(4,5),(5,5),(1,6),(2,6),(5,6),(1,7),(5,7),
   (1,8),(2,8),(5,8)], -- light red
  [(3,6),(4,6),(4,7),(8,7),(4,8),(6,8),(7,8),(8,8),
   (4,9),(5,9),(6,9)] -- light sea
  ]

mregions = Matrix.fromList 10 10 [
  1, 1, 2, 2, 2, 2, 2, 2, 3, 3,
  1, 2, 2, 1, 4, 4, 4, 4, 4, 3,
  1, 1, 1, 1, 4, 5, 3, 3, 3, 3,
  4, 4, 4, 4, 4, 5, 5, 6, 6, 6,
  4, 7, 7, 7, 7, 7, 5, 8, 8, 6,
  7, 7, 9, 9, 9, 9, 5, 8, 6, 6,
  7, 9, 9, 0, 0, 9, 5, 8, 8, 8,
  7, 9, 7, 7, 0, 9, 5, 5, 0, 8,
  7, 9, 9, 7, 0, 9, 0, 0, 0, 8,
  7, 7, 7, 7, 0, 0, 0, 8, 8, 8 ]
-}

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
regionReject :: Matrix.Matrix Int -> [(Int,Int)] -> Bool
regionReject regions solution = loop solution (Vector.replicate 10 0)
  where loop [] counts = True --Vector.all (==2) counts
        loop ((x,y):posns) counts =
          let regionId = Matrix.unsafeGet (y+1) (x+1) regions
              current = counts Vector.! regionId
          in if current < 2
             then loop posns (counts Vector.// [(regionId, current + 1)])
             else False
-- -}

 {- 
regionReject :: [Set.Set (Int,Int)] -> [(Int,Int)] -> Bool
regionReject regions ns = all (region (Set.fromList ns)) regions

region :: Set.Set (Int,Int) -> Set.Set (Int,Int) -> Bool
region a b = Set.size (Set.intersection a b) == 2
-- -}

