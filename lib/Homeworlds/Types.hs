{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Homeworlds.Types where

import Core
import GameTree

import qualified Data.Aeson         as A
import qualified Data.IntMap.Strict as IntMap
import qualified Text.Show          as Show


-- Game State ------------------------------------------------------------------------------------------------

data Color = Red | Green | Blue | Yellow
  deriving (Eq, Ord, Enum, Bounded)

data Size = Small | Medium | Large
  deriving (Eq, Ord, Enum, Bounded)

data Piece = Piece { pieceColor ∷ !Color, pieceSize ∷ !Size }
  deriving (Eq, Ord, Bounded)

data Ship = Ship { shipOwner ∷ !PlayerId, shipPiece ∷ !Piece }
  deriving (Eq, Ord)

data Star = Binary !Piece !Piece
          | Single !Piece
  deriving (Eq, Ord, Show)

data System = System { systemStar ∷ !Star, systemDocked ∷ ![Ship] }
  deriving (Eq, Ord, Show)

type PlayerId = Int
type SystemId = Int

data GameSt = GameSt { gameStReserve    ∷ !(IntMap Int)
                     , gameStSystems    ∷ !(IntMap System)
                     , gameStPlayerToMv ∷ !PlayerId
                     , gameStNumSystems ∷ !Int
                     , gameStNumPlayers ∷ !Int
                     , gameStLosers     ∷ ![PlayerId]
                     }
  deriving (Eq, Ord, Show)


-- Events ----------------------------------------------------------------------------------------------------

data Setup = Setup !Piece !Piece !Color
  deriving (Eq, Ord, Show)

data Destination = Existing !SystemId
                 | Fresh !Piece
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


-- Moves, Games, and GameTrees -------------------------------------------------------------------------------

type HWMove = Move GameSt
type HWGame = Game GameSt
type HWTree = GameTree Event GameSt


-- Instances -------------------------------------------------------------------------------------------------

intMapList ∷ IntMap a → [Maybe a]
intMapList m | IntMap.null m = []
intMapList m              = IntMap.findMax m & \case
  (k,_) → flip IntMap.lookup m <$> [0..k]

numberOfSizes ∷ Int
numberOfSizes = 1 + fromEnum (maxBound ∷ Size)

instance Enum Piece where
  enumFrom pc = enumFromTo pc maxBound
  enumFromThen pc pc2 = enumFromThenTo pc pc2 maxBound
  fromEnum (Piece pc sz) = fromEnum sz + (fromEnum pc * numberOfSizes)
  toEnum i = Piece pc sz
    where sz = toEnum $ i `mod` numberOfSizes
          pc = toEnum $ i `div` numberOfSizes


instance Show Color where show = \case { Red → "r"; Green → "g"; Blue → "b"; Yellow → "y" }
instance Show Size where show = \case { Small → "1"; Medium → "2"; Large → "3" }
instance Show Piece where show (Piece color size) = Show.show color <> Show.show size
instance Show Ship where show (Ship player piece) = "\"" <> Show.show piece <> "@" <> Show.show player <> "\""
instance A.ToJSON Color where toJSON = A.toJSON . Show.show
instance A.FromJSON Color where parseJSON = undefined
instance A.ToJSON Piece where toJSON = A.toJSON . Show.show
instance A.FromJSON Piece where parseJSON = undefined

makeFields ''Piece
makeFields ''Ship
makeFields ''Star
makeFields ''System
makeFields ''GameSt
makeFields ''Setup
makeFields ''Action
makeFields ''Event

$(deriveJSON ''Size)
$(deriveJSON ''Star)
$(deriveJSON ''Ship)
$(deriveJSON ''System)
$(deriveJSON ''GameSt)
$(deriveJSON ''Setup)
$(deriveJSON ''Destination)
$(deriveJSON ''Action)
$(deriveJSON ''Event)
