module Homeworlds.Game where

import Core
import Homeworlds.Move
import Homeworlds.Types hiding (systems)

import Control.Monad.State.Lazy


-- General Utilities -----------------------------------------------------------------------------------------

systems ∷ HWGame SystemId
systems = do
  n ← use numSystems
  lift [0 .. n-1]

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
  pc   ← lift $ map (view piece) $ filter ((pl ==) . view owner) $ view ships sys
  dest ← destinations
  return $ Move loc pc dest

attacks ∷ HWGame Action
attacks = do
  loc    ← systems
  pl     ← fromMove (use playerToMv)
  sys    ← fromMove (lookupSystem loc)
  target ← lift $ filter ((pl /=) . view owner) $ view ships sys
  return $ Attack loc target

trades ∷ HWGame Action
trades = do
  loc    ← systems
  sys    ← fromMove (lookupSystem loc)
  pl     ← fromMove (use playerToMv)
  target ← lift $ filter ((pl ==) . view owner) $ view ships sys
  colr   ← lift $ filter (/= target^.piece.color) enumeration
  return $ Trade loc (target ^. piece) colr

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
  target ← lift $ filter ((pl ==) . view owner) $ view ships sys
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

fromMove ∷ HWMove a → HWGame a
fromMove = hoist toList

joins ∷ HWGame Event
joins = setup >>= fromMove . applyEvent . Join

turns ∷ HWGame Event
turns = actionSeqs >>= fromMove . applyEvent . Turn

events ∷ HWGame Event
events = join $ lift [joins, turns]
