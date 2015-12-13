module TTT.Main where

import Core
import Game.Tree
import TTT.Move
import TTT.Types

import Control.Monad.State.Lazy


fromMove ∷ Move GameSt a → Game GameSt a
fromMove = hoist toList

moves ∷ TTTGame Event
moves = do
  fromMove assertNoWinner
  x ← lift [0..2]
  y ← lift [0..2]
  let e = Place (Tile x y)
  fromMove $ place (Tile x y)
  return e


-- Tie it all together ---------------------------------------------------------------------------------------

someArbitraryState ∷ GameSt
Just someArbitraryState = flip execStateT initialState $ do
    place (Tile 1 1)
    place (Tile 0 0)
    place (Tile 1 0)

main ∷ IO ()
main = do
    randomGame (gameTree initialState moves) >>= \case
        GameTrace initialSt history → do
            print initialSt
            forM_ history $ \(e,st) → print e >> print st

    when False $ do
      print $ (length . flip statesAtDepth (gameTree initialState moves)) <$> [0..9]

      -- print $ length $ reachableStatesWherePlayerWinsAfterThis X initialState
      -- print $ length $ reachableStatesWherePlayerWinsAfterThis X someArbitraryState
      -- print $ length $ reachableStatesWherePlayerWinsAfterThis O someArbitraryState
