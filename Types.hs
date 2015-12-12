{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Types where

import           Control.Lens
import           Control.Lens.TH
import           Control.Monad
import           Control.Monad.List
import           Control.Monad.State.Lazy
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           Data.Char                  (toLower)
import           Data.Foldable
import qualified Data.List                  as List
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                   as Set
import           Debug.Trace
import           System.Random.Shuffle      (shuffleM)
import           Text.Show.Pretty           (ppShow)

import           THUtils


-- Game State ------------------------------------------------------------------------------------------------

data Color = Red | Green | Blue | Yellow
  deriving (Eq, Ord, Enum, Bounded)

data Size = Small | Medium | Large
  deriving (Eq, Ord, Enum, Bounded)

data Piece = Piece { _color ∷ !Color, _size ∷ !Size }
  deriving (Eq, Ord)

data Ship = Ship { _owner ∷ !PlayerId, _piece ∷ !Piece }
  deriving (Eq, Ord)

data Star = Binary !Piece !Piece
          | Single !Piece
  deriving (Eq, Ord, Show)

data System = System { _star ∷ Star, _ships ∷ [Ship] }
  deriving (Eq, Ord, Show)

type PlayerId = Int
type SystemId = Int

data GameSt = GameSt { _reserve    ∷ (Map Piece Int)
                     , _systems    ∷ (Map SystemId System)
                     , _turn       ∷ !PlayerId
                     , _numSystems ∷ !Int
                     , _numPlayers ∷ !Int
                     , _history    ∷ [Event]
                     }
  deriving (Eq, Ord, Show)


-- Events ----------------------------------------------------------------------------------------------------

data Setup = Setup Piece Piece Color
  deriving (Eq, Ord, Show)

data Destination = Existing SystemId
                 | Fresh Piece
  deriving (Eq, Ord, Show)

data Action = Construct !SystemId !Color
            | Trade !SystemId !Piece !Color
            | Move !SystemId !Piece !Destination
            | Attack !SystemId !Ship
            | Sacrifice !SystemId !Piece
            | Catastrophe !SystemId !Color
  deriving (Eq, Ord, Show)

data Event = Join !Setup
           | Resign
           | Turn ![Action]
  deriving (Eq, Ord, Show)


-- The Game and Move Monads ----------------------------------------------------------------------------------

type GameT = StateT GameSt
type MoveM = GameT Maybe
type GameM = GameT []


-- Instances -------------------------------------------------------------------------------------------------

intMapList ∷ Map Int a → [Maybe a]
intMapList m | Map.null m = []
intMapList m              = Map.findMax m & \case
  (k,_) → flip Map.lookup m <$> [0..k]

instance Show Color where show = \case { Red → "r"; Green → "g"; Blue → "b"; Yellow → "y" }
instance Show Size where show = \case { Small → "1"; Medium → "2"; Large → "3" }
instance Show Piece where show (Piece color size) = show color <> show size
instance Show Ship where show (Ship player piece) = "\"" <> show piece <> "@" <> show player <> "\""
instance A.ToJSON v => A.ToJSON (Map Int v) where toJSON = A.toJSON . intMapList
instance A.FromJSON v => A.FromJSON (Map Int v) where parseJSON v = Map.fromList <$> A.parseJSON v
instance A.ToJSON Color where toJSON = A.toJSON . show
instance A.FromJSON Color where parseJSON = undefined
instance A.ToJSON Piece where toJSON = A.toJSON . show
instance A.FromJSON Piece where parseJSON = undefined
instance A.ToJSON v => A.ToJSON (Map Piece v) where toJSON = A.toJSON . Map.mapKeys show
instance A.FromJSON v => A.FromJSON (Map Piece v) where parseJSON v = undefined -- Map.fromList <$> A.parseJSON v

makeLenses ''Piece
makeLenses ''Ship
makeLenses ''Star
makeLenses ''System
makeLenses ''GameSt
makeLenses ''Setup
makeLenses ''Action
makeLenses ''Event

$(deriveJSON ''Size)
$(deriveJSON ''Star)
$(deriveJSON ''Ship)
$(deriveJSON ''System)
$(deriveJSON ''GameSt)
$(deriveJSON ''Setup)
$(deriveJSON ''Destination)
$(deriveJSON ''Action)
$(deriveJSON ''Event)
