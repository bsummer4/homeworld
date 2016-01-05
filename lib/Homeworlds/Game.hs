module Homeworlds.Game where

import Core
import Homeworlds.Move
import Homeworlds.Types hiding (systems)
import qualified Homeworlds.Types as Types

import           Control.Monad.State.Lazy
import qualified Data.IntMap              as IntMap


-- General Utilities -----------------------------------------------------------------------------------------

systems ∷ HWGame SystemId
systems = IntMap.keys <$> use Types.systems >>= lift

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
  pl   ← fromMove (use playerToMv)
  sys  ← fromMove (lookupSystem loc)
  pc   ← lift $ map (view piece) $ filter ((pl ==) ◁ view owner) $ view docked sys
  dest ← destinations
  return $ Move loc pc dest

attacks ∷ HWGame Action
attacks = do
  loc    ← systems
  pl     ← fromMove (use playerToMv)
  sys    ← fromMove (lookupSystem loc)
  target ← lift $ filter ((pl /=) ◁ view owner) $ view docked sys
  return $ Attack loc target

trades ∷ HWGame Action
trades = do
  loc    ← systems
  sys    ← fromMove (lookupSystem loc)
  pl     ← fromMove (use playerToMv)
  target ← lift $ filter ((pl ==) ◁ view owner) $ view docked sys
  colr   ← lift $ filter (/= target^.piece.color) enumeration
  return $ Trade loc (target^.piece) colr

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
  sys    ← fromMove (lookupSystem loc)
  pl     ← fromMove (use playerToMv)
  target ← lift $ filter ((pl ==) ◁ view owner) $ view docked sys
  return $ Sacrifice loc (target ^. piece)

actions ∷ HWGame Action
actions = join $ lift [moves, constructions, attacks, trades]

simpleActionSeqs ∷ HWGame [Action]
simpleActionSeqs = do
  action ← join $ lift [actions, sacrifices]
  case action of
    Sacrifice _loc pc → fmap (action:) $ replicateM (piecePower pc) $ case (view color pc) of
      Red    → attacks
      Green  → constructions
      Blue   → trades
      Yellow → moves
    _ → return [action]

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
  return (Setup p1 p2 c)


-- Execute Arbitrary Actions ---------------------------------------------------------------------------------

fromMove ∷ HWMove a → HWGame a
fromMove = hoist toList

joins ∷ HWGame Event
joins = do
  fromMove assumingInJoinPhase
  s ← setup
  fromMove $ applyEvent $ Join s

turns ∷ HWGame Event
turns = do
  fromMove assumingInTurnPhase
  acts ← actionSeqs
  fromMove $ applyEvent $ Turn acts

events ∷ HWGame Event
events = do
  action ← lift [joins, turns]
  action
