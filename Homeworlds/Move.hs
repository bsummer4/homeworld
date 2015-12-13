{-# LANGUAGE Rank2Types #-}

module Homeworlds.Move where

import Core
import Homeworlds.Types

import           Control.Monad.State.Lazy
import qualified Data.IntMap.Strict       as IntMap
import qualified Data.List                as List


-- Initial States --------------------------------------------------------------------------------------------

-- This is three rainbow stashes.
startingDeck ∷ IntMap Int
startingDeck = IntMap.fromList $ do
  colr <- enumeration
  sz <- enumeration
  return (fromEnum $ Piece colr sz, 9)

emptyState ∷ GameSt
emptyState = GameSt
  { _reserve    = startingDeck
  , _systems    = mempty
  , _playerToMv = 0
  , _numSystems = 0
  , _numPlayers = 0
  , _losers     = []
  }


-- Questions about the game state ----------------------------------------------------------------------------

starMakeup ∷ Star → [Piece]
starMakeup = \case { Single x → [x]; Binary x y → [x,y] }

inBank ∷ Piece → Lens' GameSt Int
inBank pc = reserve . at (fromEnum pc) . non 0

starColors ∷ Fold System Color
starColors = star . to starMakeup . traverse . color

dockedColors ∷ Fold System Color
dockedColors = ships . traverse . piece . color

isAHomeworld ∷ SystemId → Getter GameSt Bool
isAHomeworld loc = numPlayers . to (loc >=)

isOwnedBy ∷ Ship → PlayerId → Bool
s `isOwnedBy` p = s ^. owner == p

colorsAvailableFor ∷ PlayerId → System → [Color]
colorsAvailableFor pid system = (system ^.. starColors)
                             ++ (view (piece . color) <$> filter (`isOwnedBy` pid) (system ^. ships))

systemPieces ∷ System → [Piece]
systemPieces sys = (sys ^. star . to starMakeup) <> (sys ^.. ships . traverse . piece)

pieceAvailable ∷ Piece → Getter GameSt Bool
pieceAvailable pc = inBank pc . to (> 0)

smallestInBank ∷ Color → GameSt → Maybe Size
smallestInBank c st = safeHead sizesAvailable
  where
    sizeAvailability = enumeration <&> \sz → st ^. pieceAvailable (Piece c sz) . to (sz,)
    sizesAvailable = fst <$> filter snd sizeAvailability


--------------------------------------------------------------------------------------------------------------

assuming ∷ Getter GameSt Bool → HWMove ()
assuming cond = use cond >>= guard

grabPc ∷ Piece → HWMove Piece
grabPc pc = do
  assuming $ inBank pc . to (>= 1)
  reserve . at (fromEnum pc) %= Just . (+1) . fromMaybe 0
  return pc

grabShip ∷ Ship → HWMove Ship
grabShip ship = grabPc (ship ^. piece) >> return ship

grabStar ∷ Star → HWMove Star
grabStar theStar = mapM_ grabPc (starMakeup theStar) >> return theStar

grabSmallestShipFromReserve ∷ Color → HWMove Piece
grabSmallestShipFromReserve c = do
  sz ← lift . smallestInBank c =<< get
  grabPc (Piece c sz)

grabSystem ∷ System → HWMove SystemId
grabSystem sys = do
  _ ← grabStar (sys ^. star)
  mapM_ grabShip (sys ^. ships)
  newId ← use numSystems
  numSystems         .= (newId + 1)
  systems . ix newId .= sys
  return newId

grabPlayer ∷ HWMove PlayerId
grabPlayer = do
  playerId ← use numPlayers
  numPlayers %= (+1)
  return playerId

grabOrLookupDestination ∷ Destination → HWMove SystemId
grabOrLookupDestination = \case
  Existing loc → return loc
  Fresh pc     → grabSystem $ System (Single pc) []


-- Setup -----------------------------------------------------------------------------------------------------

joinGame ∷ Setup → HWMove PlayerId
joinGame (Setup s1 s2 c) = do
  playerId ← grabPlayer
  _ ← grabSystem $ System (Binary s1 s2) [Ship playerId (Piece c Large)]
  return playerId


-- Game Logic Basics -----------------------------------------------------------------------------------------

lookupSystem ∷ SystemId → HWMove System
lookupSystem loc = do
  mSys ← use (systems . at loc)
  lift mSys

shipsAt ∷ SystemId → (Ship → Bool) → HWMove [Ship]
shipsAt loc f = do
  sys ← lookupSystem loc
  return $ filter f (sys ^. ships)

piecesAt ∷ SystemId → (Piece → Bool) → HWMove [Piece]
piecesAt loc f = do
  sys ← lookupSystem loc
  return $ filter f $ systemPieces sys

shipsExistAt ∷ SystemId → (Ship → Bool) → HWMove Bool
shipsExistAt loc f = not . null <$> shipsAt loc f

shipExistsAt ∷ SystemId → Ship → HWMove Bool
shipExistsAt loc ship = shipsExistAt loc (== ship)

shipSize ∷ Ship → Size
shipSize = view (piece . size)

biggestShip ∷ [Ship] → (Maybe Ship)
biggestShip = safeHead . reverse . List.sortOn shipSize

ourLargestShipAt ∷ SystemId → HWMove Ship
ourLargestShipAt loc = do
  us ← use playerToMv
  mShip ← biggestShip <$> shipsAt loc (\ship → us == view owner ship)
  lift mShip

ownsAShipWith ∷ SystemId → PlayerId → Color → HWMove Bool
ownsAShipWith loc player colr =
  shipsExistAt loc $ \(Ship ownr (Piece c _)) →
    and [ownr == player, c == colr]

homeWorld ∷ PlayerId → HWMove System
homeWorld = lookupSystem

playerLoses ∷ PlayerId → HWMove ()
playerLoses p = do
  losers %= (p:)

handleWinConditions ∷ HWMove ()
handleWinConditions = do
  playerCount ← use numPlayers
  forM_ [0 .. playerCount-1] $ \pid → do
    mSys ← use (systems . at pid)
    mSys & \case
      Nothing  → playerLoses pid
      Just sys → when (null $ filter (`isOwnedBy` pid) (sys ^. ships)) $
                   playerLoses pid

advanceTurn ∷ HWMove ()
advanceTurn = do
  playerCount ← use numPlayers
  handleWinConditions
  playerToMv %= \pid → (pid + 1) `mod` playerCount

placeShip ∷ SystemId → Ship → HWMove ()
placeShip loc p = do
  sys ← lookupSystem loc
  systems . at loc .= Just (over ships (p:) sys)

deleteShip ∷ SystemId → Ship → HWMove ()
deleteShip loc ship = do
  guard =<< shipExistsAt loc ship
  modify $ over systems
         $ flip IntMap.update loc
         $ Just . (over ships (List.delete ship))


-- Validate Action Sequences ---------------------------------------------------------------------------------

piecePower ∷ Piece → Int
piecePower pc = view size pc & \case { Small → 1; Medium → 2; Large → 3 }

validActionSequence ∷ [Action] → Bool
validActionSequence = checkValid
  where
    isCatastrophe ∷ Action → Bool
    isCatastrophe = \case { Catastrophe _ _ → True; _ → False }

    checkValid ∷ [Action] → Bool
    checkValid = \case
      []                       → False
      Construct _ _    : more → all isCatastrophe more
      Move _ _ _       : more → all isCatastrophe more
      Attack _ _       : more → all isCatastrophe more
      Trade _ _ _      : more → all isCatastrophe more
      Sacrifice _ p    : more → checkSacrifice (p^.color) (piecePower p) more
      Catastrophe _ _  : more → validActionSequence more

    checkSacrifice ∷ Color → Int → [Action] → Bool
    checkSacrifice _ 0     = all isCatastrophe
    checkSacrifice c power = \case
      []                     → False
      Catastrophe _ _ : more → checkSacrifice c power more
      Sacrifice _ _   : _    → False
      Construct _ _   : more → c == Green  && checkSacrifice c (power - 1) more
      Move _ _ _      : more → c == Yellow && checkSacrifice c (power - 1) more
      Attack _ _      : more → c == Red    && checkSacrifice c (power - 1) more
      Trade _ _ _     : more → c == Blue   && checkSacrifice c (power - 1) more


-- Implementations for Moves ---------------------------------------------------------------------------------

actionAvailable ∷ SystemId → Color → HWMove Bool
actionAvailable loc col = do
  sys ← lookupSystem loc
  pid ← use playerToMv
  return $ col `elem` colorsAvailableFor pid sys

noneEqual ∷ Eq a ⇒ [a] → Bool
noneEqual [] = True
noneEqual (x:xs) = all (/= x) xs && noneEqual xs

canTeleportBetween ∷ System → System → Bool
canTeleportBetween sys1 sys2 =
  noneEqual $ map (view size) $ concat $ map (starMakeup . view star) [sys1,sys2]

canMoveTo ∷ SystemId → SystemId → HWMove Bool
canMoveTo start dest =
  if start == dest
    then return False
    else canTeleportBetween <$> lookupSystem start <*> lookupSystem dest

constructShip ∷ SystemId → Color → HWMove ()
constructShip loc colr = do
  player ← use playerToMv
  guard =<< ownsAShipWith loc player colr
  guard =<< actionAvailable loc colr
  pc ← grabSmallestShipFromReserve colr
  placeShip loc $ Ship player pc

attackShip ∷ SystemId → Ship → HWMove ()
attackShip loc target = do
  player ← use playerToMv
  guard =<< actionAvailable loc Red
  guard =<< shipExistsAt loc target
  guard $ view owner target /= player

  attacker ← ourLargestShipAt loc
  guard $ target^.piece.size <= attacker^.piece.size

  deleteShip loc target
  _ ← grabPc (view piece target)
  placeShip loc $ Ship player (view piece target)

tradeShip ∷ SystemId → Piece → Color → HWMove ()
tradeShip loc pc newColor = do
  player ← use playerToMv
  let oldShip = Ship player pc
  guard =<< actionAvailable loc Blue
  guard =<< shipExistsAt loc oldShip
  let newShip = set (piece . color) newColor oldShip
  deleteShip loc oldShip
  _ ← grabPc (newShip ^. piece)
  placeShip loc newShip

sacrificeShip ∷ SystemId → Piece → HWMove ()
sacrificeShip loc pc = do
  player ← use playerToMv
  let ship = Ship player pc
  guard =<< shipExistsAt loc ship
  deleteShip loc ship

moveShip ∷ SystemId → Piece → Destination → HWMove ()
moveShip loc pc dest = do
  player ← use playerToMv
  guard =<< actionAvailable loc Yellow
  destId ← grabOrLookupDestination dest
  guard =<< canMoveTo loc destId
  let ship = Ship player pc
  deleteShip loc ship
  _ ← grabPc (ship ^. piece)
  placeShip destId ship

systemStarColors ∷ SystemId → HWMove [Color]
systemStarColors loc = do
  sys ← lookupSystem loc
  return $ view color <$> starMakeup (view star sys)

-- TODO Add everything back to the bank.
destroySystem ∷ SystemId → HWMove ()
destroySystem = modify . over systems . IntMap.delete

destroyStarsWithColor ∷ SystemId → Color → HWMove ()
destroyStarsWithColor loc c = do
  thisStar ← view star <$> lookupSystem loc

  let starPieces = starMakeup thisStar
      remaining  = filter (\p → p^.color /= c) starPieces

  guard $ length starPieces /= length remaining
  case remaining of
    (_:_:_) → error "This will never happen"
    []      → destroySystem loc
    [p]     → modify $ over systems
                     $ flip IntMap.update loc
                     $ Just . set star (Single p)

causeCatastrophe ∷ SystemId → Color → HWMove ()
causeCatastrophe loc c = do
  fuel ← piecesAt loc (\p → c == p^.color)
  guard $ length fuel >= 4

  -- TODO This would be a lot less confusing if we just got the system,
  --      and then did all of our operations on that pure value.

  -- It's important that we do this before we kill the star, otherwise we
  -- might try to delete ships that no longer exist.
  targets ← shipsAt loc (\p → c == p^.piece.color)
  forM_ targets $ do
    deleteShip loc

  starAffected ← (c `elem`) <$> systemStarColors loc
  when starAffected $ do
    destroyStarsWithColor loc c

cleanup ∷ HWMove ()
cleanup = do
  sysKVPairs ← use (systems . to IntMap.toList)
  forM_ sysKVPairs $ \(loc, sys) → do
    hw ← use (isAHomeworld loc)
    when (not hw && null (sys ^. ships)) $ do
      destroySystem loc

applyAction ∷ Action → HWMove ()
applyAction act = do
  act & \case
    Construct   loc c        → constructShip loc c
    Move        loc p dest   → moveShip loc p dest
    Attack      loc target   → attackShip loc target
    Trade       loc target c → tradeShip loc target c
    Sacrifice   loc target   → sacrificeShip loc target
    Catastrophe loc colr     → causeCatastrophe loc colr
  cleanup

applyEvent ∷ Event → HWMove Event
applyEvent ev = do
  assuming (losers . to null)
  ev & \case
    Join setup   → void (joinGame setup)
    Resign       → use playerToMv >>= playerLoses
    Turn actions → do guard $ validActionSequence actions
                      sequence_ (applyAction <$> actions)
                      advanceTurn
  return ev
