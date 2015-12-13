module HWGame where

import GamePrelude

import           Control.Lens
import           Control.Lens.TH
import           Control.Monad
import           Control.Monad.List
import           Control.Monad.Morph        (hoist)
import           Control.Monad.State.Lazy
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           Data.Char                  (toLower)
import           Data.Foldable
import qualified Data.List                  as List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                   as Set
import           Debug.Trace
import           System.Random.Shuffle      (shuffleM)
import           Text.Show.Pretty           (ppShow)

import Types hiding (systems)
import HWMove


-- General Utilities -----------------------------------------------------------------------------------------

systems ∷ HWGame SystemId
systems = do
  numSystems ← view numSystems <$> get
  lift [0 .. numSystems-1]

pieces ∷ HWGame Piece
pieces = Piece <$> lift enumeration <*> lift enumeration

destinations ∷ HWGame Destination
destinations = do
  freshDest ← lift [True, False]
  if freshDest then Fresh    <$> pieces
               else Existing <$> systems

moves ∷ HWGame Action
moves = do
  loc  ← systems
  pl   ← fromMove currentPlayer
  sys  ← fromMove (getSystem loc)
  pc   ← lift $ map (view piece) $ filter ((pl ==) . view owner) $ view ships sys
  dest ← destinations
  return $ Move loc pc dest

attacks ∷ HWGame Action
attacks = do
  loc    ← systems
  pl     ← fromMove currentPlayer
  sys    ← fromMove (getSystem loc)
  target ← lift $ filter ((pl /=) . view owner) $ view ships sys
  return $ Attack loc target

trades ∷ HWGame Action
trades = do
  loc    ← systems
  sys    ← fromMove (getSystem loc)
  pl     ← fromMove currentPlayer
  target ← lift $ filter ((pl ==) . view owner) $ view ships sys
  color  ← lift $ filter (/= target^.piece.color) enumeration
  return $ Trade loc (target ^. piece) color

constructions ∷ HWGame Action
constructions = Construct <$> systems <*> lift enumeration

catastrophes ∷ HWGame Action
catastrophes = do
  loc     ← systems
  c       ← lift enumeration
  targets ← fromMove $ piecesAt loc (\p → c == p^.color)
  guard $ length targets >= 4
  return (Catastrophe loc c)

sacrifices ∷ HWGame Action
sacrifices = do
  loc    ← systems
  sys    ← fromMove (getSystem loc)
  pl     ← fromMove currentPlayer
  target ← lift $ filter ((pl ==) . view owner) $ view ships sys
  return $ Sacrifice loc (target ^. piece)

actions ∷ HWGame Action
actions = join $ lift [moves, constructions, attacks, trades]

simpleActionSeqs ∷ HWGame [Action]
simpleActionSeqs = do
  action ← join $ lift [actions, sacrifices]
  case action of
    Sacrifice loc pc → fmap (action:) $ replicateM (piecePower pc) $ case (view color pc) of
                           Red    → attacks
                           Green  → constructions
                           Blue   → trades
                           Yellow → moves
    _                → return [action]

actionSeqs ∷ HWGame [Action]
actionSeqs = do
  let possibleCatastrophes = evalStateT catastrophes <$> get
  possibleCatastrophes >>= \case
    []   → simpleActionSeqs
    evil → (evil ++) <$> simpleActionSeqs -- TODO

setup ∷ HWGame Setup
setup = do
  p1 ← pieces
  p2 ← pieces
  guard $ p1 >= p2
  c ← lift enumeration

  -- Just to constrain the space
  guard $ p1^.size /= p2^.size
  guard $ p1^.color /= p2^.color
  guard $ c         /= p2^.color
  guard $ p1^.color /= c

  return (Setup p1 p2 c)


-- Execute Arbitrary Actions ---------------------------------------------------------------------------------

joins ∷ HWGame Event
joins = setup >>= fromMove . applyEvent . Join

turns ∷ HWGame Event
turns = actionSeqs >>= fromMove . applyEvent . Turn

fromMove ∷ HWMove a → HWGame a
fromMove = hoist toList
