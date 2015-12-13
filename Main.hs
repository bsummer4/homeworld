module Main where

import           Core
import           Game.Tree
import qualified Homeworlds.Game  as HW
import qualified Homeworlds.Move  as HW
import           Homeworlds.Types as HW
import qualified TTT.Main         as TTT

import           Control.Monad.Random
import           Control.Monad.State.Lazy
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           System.Random.Shuffle      (shuffleM)


-- Tie it all together. --------------------------------------------------------------------------------------

main ∷ IO ()
main = do
  TTT.main
  tr ← randomGame $ gameTree HW.emptyState HW.events
  printTrace (\st → (st ^. systems, st ^. losers)) tr
