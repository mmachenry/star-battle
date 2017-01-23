-- | Generates solutions to the well known Star Battle puzzle. Puzzles are
-- given as an NxN, square matrix of integers. The integers represent which
-- region the square is in. The region labels must be an integer in the range
-- [0 .. n - 1]. The solutions are given as a list of length 2*n of (x,y)
-- positions.

module StarBattle where

import FD (FDExpr, FD, runFD, news, labelling, fromInt, (#==), (#<), (#\=))
import Control.Monad (when, zipWithM_, mplus, mzero)
import qualified Data.Set as Set
import qualified Data.Matrix as M
import qualified Data.Vector as V

-- | Given a matrix of regions, and therefor aslo implicitely a size of the
-- grid, produce all solutions to the star battle puzzle. Solutions are 
-- produced in a lazy list so the consumer can chose to read just the first,
-- and likely only solution, or to continue reading, producing an exhaustive
-- search of the space.
starBattle :: M.Matrix Int -> [[(Int, Int)]]
starBattle regions = filter (regionReject regions) (generate (M.nrows regions))

-- Given a size for the N x N grid, produce a list of all lists of positions
-- that form a valid solution ignoring regions. That is, solutions which have
-- two stars in every colum and row and no adjacent stars.
generate :: Int -> [[(Int, Int)]]
generate size = runFD $ do
  vars1 <- news size (0,size-1)
  vars2 <- news size (0,size-1)
  columns vars1 vars2
  notAdjacent vars1 vars2
  rows size (vars1++vars2)
  v1 <- labelling vars1
  v2 <- labelling vars2
  return $ zip [0..] v1 ++ zip [0..] v2

-- Given that there are two stars in each column, the start positions are
-- produced positionally in two lists. This means that first first element
-- in each list are both stars in the first column. Element 4 in both lists
-- are the two stars in column 4, etc. Given that they cannot occupy the same
-- square, and they may not be adjacent, and that, without lose of potential
-- solutions we may assume, arbitrarily, that the first list always contains
-- the star in the lower index, we greatly reduce the possible solutions that
-- we search. Therefor, the column constraint simply ensures that the lists
-- pairwise meet the constraint that a + 1 < b. The first star in a column
-- is at least two rows higher than the second star.
columns :: [FDExpr] -> [FDExpr] -> FD ()
columns = zipWithM_ (\a b->a + 1 #< b)

-- Given a size, we check the tally of number of stars in each row. This
-- ensures that there are exactly two stars in each row. This function makes
-- a huge tree of all possible assignments of the FDExprs and is thus very
-- inefficient. I have attempted to pull this out of the constraint solver and
-- simply do post processing in a liner way but that only decreased performance
-- even though the solution was linear. The reason being that way more
-- possible results were create without this being part of the constraints
-- up front. What would be best is if I implemented this algorithm for the
-- `among` constraint in the FD library.
-- http://web.emn.fr/x-info/sdemasse/gccat/Camong.html
-- Or potentially come up with something else equally clever.
rows :: Int -> [FDExpr] -> FD ()
rows size vars = mapM_ (tally vars starsPerRow) [0 .. size-1]
  where starsPerRow = 2
        tally [] count _ = when (count /= 0) mzero
        tally (x:xs) count n =
          mplus (x #== fromInt n >> tally xs (count-1) n)
                (x #\= fromInt n >> tally xs count n)

-- Loop through all pairs and ensure that no two elements from one column are
-- adjacent to any of the other two in the next column, and so forth.
notAdjacent :: [FDExpr] -> [FDExpr] -> FD ()
notAdjacent [] [] = return ()
notAdjacent [_] [_] = return ()
notAdjacent (a1:l1@(b1:_)) (a2:l2@(b2:_)) = do
  1 #< abs(a1-b1)
  1 #< abs(a1-b2)
  1 #< abs(a2-b1)
  1 #< abs(a2-b2)
  notAdjacent l1 l2

-- True if the given solution set of positions meets the region requirement
-- on the given region. To do this efficiently, I setup an accumulator vector
-- for the number of stars per numerical region. If that number ever gets to be
-- more than 2, we havre an error. Otherwise we match the constraint. Rewriting
-- this as a true constraint within the FD proved to be a lot less performant
-- so it remains a post processing check on the possible solutions.
regionReject :: M.Matrix Int -> [(Int,Int)] -> Bool
regionReject regions solution = loop solution (V.replicate (M.nrows regions) 0)
  where loop [] counts = True
        loop ((x,y):posns) counts =
          let regionId = M.unsafeGet (y+1) (x+1) regions
              current = counts V.! regionId
          in current < 2 && loop posns (counts V.// [(regionId, current + 1)])

