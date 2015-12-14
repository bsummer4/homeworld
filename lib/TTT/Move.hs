module TTT.Move where

import Core
import TTT.Types

import           Control.Monad.State.Lazy
import qualified Data.Map                 as Map


--------------------------------------------------------------------------------------------------------------

winningRows ∷ [[Tile]]
winningRows = map (\(x,y) → Tile x y) <$>
  [ [(0,0), (1,0), (2,0)]
  , [(0,1), (1,1), (2,1)]
  , [(0,2), (1,2), (2,2)]
  , [(0,0), (0,1), (0,2)]
  , [(1,0), (1,1), (1,2)]
  , [(2,0), (2,1), (2,2)]
  , [(0,0), (1,1), (2,2)]
  , [(0,2), (1,1), (2,0)]
  ]

winnerOnRow ∷ GameSt → [Tile] → Maybe Player
winnerOnRow st idxs =
    case (\k → st ^. board . at k) <$> idxs of
      vals@(Just pl:_) → if (allEqual vals) then (Just pl) else Nothing
      _                → Nothing

winner ∷ GameSt → Maybe Player
winner st = safeHead $ catMaybes $ winnerOnRow st <$> winningRows

assertNoWinner ∷ TTTMove ()
assertNoWinner = do
  st ← get
  guard (isNothing (winner st))

flipTurn ∷ TTTMove ()
flipTurn = modify $ \case
  GameSt X b → GameSt O b
  GameSt O b → GameSt X b

writeSquare ∷ Tile → Player → TTTMove ()
writeSquare k v = do
  (GameSt t b) ← get
  guard $ isNothing $ Map.lookup k b
  put $ GameSt t (Map.insert k v b)

assertValidIdx ∷ Tile → TTTMove ()
assertValidIdx (Tile x y) = guard (and [x>=0, x<=2, y>=0, y<=2])

activePlayer ∷ TTTMove Player
activePlayer = _player <$> get

place ∷ Tile → TTTMove ()
place idx = do
  assertValidIdx idx
  c ← activePlayer
  writeSquare idx c
  flipTurn
