module TTT where

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


-- Types -----------------------------------------------------------------------------------------------------

data Player = X | O
  deriving (Eq,Ord,Show,Enum,Bounded)

data Event = Place !Int !Int
  deriving (Eq, Ord, Show)

type TTTSt = (Player, Map (Int,Int) Player)

type TTTMove = StateT TTTSt Maybe
type TTTGame = StateT TTTSt []


-- Initial State ---------------------------------------------------------------------------------------------

initialState = (X, Map.empty)
