module StarBattleSpec (spec) where

import Test.Hspec
import StarBattle
import qualified Data.Matrix as M
import Data.List (sort)

sample8x8input = M.fromList 8 8 [
  0, 1, 1, 1, 1, 2, 2, 2,
  0, 1, 1, 2, 2, 2, 2, 2,
  0, 1, 1, 2, 3, 2, 2, 2,
  0, 0, 3, 3, 3, 4, 4, 4,
  0, 3, 3, 3, 5, 4, 6, 4,
  0, 7, 3, 5, 5, 4, 6, 4,
  7, 7, 7, 5, 5, 5, 6, 6,
  7, 7, 6, 6, 6, 6, 6, 6
  ]

sample8x8solution = [
  [(0,1),(1,5),(2,1),(3,5),(4,0),(5,4),(6,0),(7,4),
  (0,3),(1,7),(2,3),(3,7),(4,2),(5,6),(6,2),(7,6)]]

-- Compare two sets of solutions irrespective of their orders.
-- Solutions can be correct regardless of which order they come in
-- and which order the positions in the solution are in. This could be done
-- more efficiently with sets but it's not very important.
shouldHave act exp = sort (map sort act) `shouldBe` sort (map sort exp)

spec :: Spec
spec = do
  describe "starBattle" $ do
    it "Solves a sample 8x8" $ do
      starBattle sample8x8input `shouldHave` sample8x8solution

