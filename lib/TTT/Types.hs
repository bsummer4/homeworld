module TTT.Types where

import Core

import Control.Monad.State.Lazy


-- Types -----------------------------------------------------------------------------------------------------

data Player = X | O
  deriving (Eq,Ord,Show,Enum,Bounded)

data Tile = Tile { _xIdx ∷ !Int
                 , _yIdx ∷ !Int }
  deriving (Eq,Ord,Show)

data Event = Place !Tile
  deriving (Eq,Ord,Show)

data GameSt = GameSt { _player ∷ !Player
                     , _board  ∷ !(Map Tile Player) }
  deriving (Eq,Ord,Show)

type TTTMove = StateT GameSt Maybe
type TTTGame = StateT GameSt []


-- Initial State ---------------------------------------------------------------------------------------------

initialState ∷ GameSt
initialState = GameSt X mempty

makeLenses ''Tile
makeLenses ''Event
makeLenses ''GameSt

instance FromJSON (Map Tile Player) where parseJSON = undefined
instance ToJSON (Map Tile Player)   where toJSON = undefined

$(deriveJSON ''Player)
$(deriveJSON ''Tile)
$(deriveJSON ''Event)
$(deriveJSON ''GameSt)
