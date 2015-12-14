module Main where

import           Core
import           GameTree
import qualified Homeworlds.Game  as HW
import qualified Homeworlds.Move  as HW
import           Homeworlds.Types as HW


main ∷ IO ()
main = do
  gameTrace ← humanGame $ gameTree HW.emptyState HW.events
  printTrace (\st → (st ^. systems, st ^. losers)) gameTrace
