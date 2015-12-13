module Main where

import           Core
import           Game.Tree
import qualified Homeworlds.Game  as HW
import qualified Homeworlds.Move  as HW
import           Homeworlds.Types as HW


main ∷ IO ()
main = do
  gameTrace ← randomGame $ gameTree HW.emptyState HW.events
  printTrace (\st → (st ^. systems, st ^. losers)) gameTrace
