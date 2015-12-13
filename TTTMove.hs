module TTTMove where

import GamePrelude

import           Control.Monad.State.Lazy
import           Data.Foldable            (length, toList)
import           Data.List                (foldl')
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Maybe
import           Data.Tree                as Tree
import           Data.Tree                (Tree, Forest)
import           System.Random.Shuffle    (shuffleM)

import TTT


--------------------------------------------------------------------------------------------------------------

winningRows ∷ [[(Int,Int)]]
winningRows = [ [(0,0), (1,0), (2,0)]
              , [(0,1), (1,1), (2,1)]
              , [(0,2), (1,2), (2,2)]
              , [(0,0), (0,1), (0,2)]
              , [(1,0), (1,1), (1,2)]
              , [(2,0), (2,1), (2,2)]
              , [(0,0), (1,1), (2,2)]
              , [(0,2), (1,1), (2,0)]
              ]

winnerOnRow ∷ TTTSt → [(Int,Int)] → Maybe Player
winnerOnRow (_,board) idxs =
    case (\k → Map.lookup k board) <$> idxs of
      vals@(Just pl:_) → if (allEqual vals) then (Just pl) else Nothing
      _                → Nothing

winner ∷ TTTSt → Maybe Player
winner st = safeHead $ catMaybes $ winnerOnRow st <$> winningRows

assertNoWinner ∷ TTTMove ()
assertNoWinner = do
  st ← get
  guard (isNothing (winner st))

flipTurn ∷ TTTMove ()
flipTurn = modify $ \case
  (X,b) → (O,b)
  (O,b) → (X,b)

writeSquare ∷ (Int,Int) → Player → TTTMove ()
writeSquare k v = do
  (t,b) ← get
  guard $ isNothing $ Map.lookup k b
  put (t, Map.insert k v b)

assertValidIdx ∷ (Int, Int) → TTTMove ()
assertValidIdx (x,y) = guard (and [x>=0, x<=2, y>=0, y<=2])

activePlayer ∷ TTTMove Player
activePlayer = fst <$> get

place ∷ (Int, Int) → TTTMove ()
place idx = do
  assertValidIdx idx
  c ← activePlayer
  writeSquare idx c
  flipTurn
