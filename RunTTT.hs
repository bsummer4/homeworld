module RunTTT where

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
import TTTMove
import GameTree


fromMove = hoist toList

moves ∷ TTTGame Event
moves = do
  fromMove assertNoWinner
  x ← lift [0..2]
  y ← lift [0..2]
  let e = Place x y
  fromMove $ place (x,y)
  return e


-- Tie it all together ---------------------------------------------------------------------------------------

someArbitraryState ∷ TTTSt
Just someArbitraryState = flip execStateT initialState $ do
    place (1,1)
    place (0,0)
    place (1,0)

main = do
    randomGame (gameTree initialState moves) >>= \case
        Trace initialSt history → do
            print initialSt
            forM_ history $ \(e,st) → print e >> print st

    when False $ do
      print $ (length . flip statesAtDepth (gameTree initialState moves)) <$> [0..9]

      -- print $ length $ reachableStatesWherePlayerWinsAfterThis X initialState
      -- print $ length $ reachableStatesWherePlayerWinsAfterThis X someArbitraryState
      -- print $ length $ reachableStatesWherePlayerWinsAfterThis O someArbitraryState
