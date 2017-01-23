module ParseMatrix (parseMatrix) where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Data.Csv (decode, HasHeader(NoHeader))

-- | Read a Matrix from a comma-separated value format. Values are expected
-- as a bytestring with no header.
parseMatrix :: Read a => BL.ByteString -> M.Matrix a
parseMatrix input =
  case decode NoHeader input of
    Left err -> error (show err)
    Right vov ->
      let lolos = map V.toList (V.toList vov)
          loloa = map (map read) lolos
      in M.fromLists loloa

