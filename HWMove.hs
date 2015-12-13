{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module HWMove where

import           GamePrelude

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
import           Data.IntMap.Strict         (IntMap)
import qualified Data.IntMap.Strict         as IntMap
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                   as Set
import           Debug.Trace
import           System.Random.Shuffle      (shuffleM)
import           Text.Show.Pretty           (ppShow)

import Types


-- Initial States --------------------------------------------------------------------------------------------

-- This is three rainbow stashes.
startingDeck ∷ IntMap Int
startingDeck = IntMap.fromList $ do
  color <- enumeration
  size <- enumeration
  return (fromEnum $ Piece color size, 9)

emptyState ∷ GameSt
emptyState = GameSt
  { _reserve    = startingDeck
  , _systems    = IntMap.empty
  , _playerToMv = 0
  , _numSystems = 0
  , _numPlayers = 0
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

colorsAvailable ∷ System → [Color]
colorsAvailable system = (system ^.. starColors) ++ (system ^.. dockedColors)

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
grabStar star = mapM_ grabPc (starMakeup star) >> return star

grabSmallestShipFromReserve ∷ Color → HWMove Piece
grabSmallestShipFromReserve c = do
  sz ← lift . smallestInBank c =<< get
  grabPc (Piece c sz)

grabSystem ∷ System → HWMove SystemId
grabSystem sys@(System star ships) = do
  grabStar star
  mapM_ grabShip ships
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
  grabSystem $ System (Binary s1 s2) [Ship playerId (Piece c Large)]
  return playerId


-- Game Logic Basics -----------------------------------------------------------------------------------------

lookupSystem ∷ SystemId → HWMove System
lookupSystem loc = do
  mSys ← use (systems . at loc)
  lift mSys

shipsAt ∷ SystemId → (Ship → Bool) → HWMove [Ship]
shipsAt loc pred = do
  sys ← lookupSystem loc
  return $ filter pred (sys ^. ships)

piecesAt ∷ SystemId → (Piece → Bool) → HWMove [Piece]
piecesAt loc pred = do
  sys ← lookupSystem loc
  return $ filter pred $ systemPieces sys

shipsExistAt ∷ SystemId → (Ship → Bool) → HWMove Bool
shipsExistAt loc pred = not . null <$> shipsAt loc pred

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
ownsAShipWith loc player color =
  shipsExistAt loc $ \(Ship owner (Piece c _)) →
    and [owner == player, c == color]

homeWorld ∷ PlayerId → HWMove System
homeWorld = lookupSystem

advanceTurn ∷ HWMove ()
advanceTurn =
  modify $ \st → set playerToMv ((view playerToMv st + 1) `mod` (view numPlayers st)) st

placeShip ∷ SystemId → Ship → HWMove ()
placeShip loc p = modify $ \st →
  st & systems .~ IntMap.update f loc (st ^. systems)
    where
      f sys = Just $ over ships (p:) sys

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
      Construct _ _     : more → all isCatastrophe more
      Move _ _ _        : more → all isCatastrophe more
      Attack _ _        : more → all isCatastrophe more
      Trade _ _ _       : more → all isCatastrophe more
      s@(Sacrifice l p) : more → checkSacrifice (p^.color) (piecePower p) more
      Catastrophe _ _   : more → validActionSequence more

    checkSacrifice ∷ Color → Int → [Action] → Bool
    checkSacrifice c 0     = all isCatastrophe
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
  return $ col `elem` colorsAvailable sys

noneEqual ∷ Eq a ⇒ [a] → Bool
noneEqual [] = True
noneEqual (x:xs) = all (/= x) xs && noneEqual xs

canTeleportBetween ∷ System → System → Bool
canTeleportBetween sys1 sys2 =
  noneEqual $ map (view size) $ concat $ map (starMakeup . view star) [sys1,sys2]

canMoveTo ∷ SystemId → SystemId → HWMove Bool
canMoveTo from to =
  if from == to
    then return False
    else canTeleportBetween <$> lookupSystem from <*> lookupSystem to

constructShip ∷ SystemId → Color → HWMove ()
constructShip loc color = do
  player ← use playerToMv
  guard =<< ownsAShipWith loc player color
  guard =<< actionAvailable loc color
  pc ← grabSmallestShipFromReserve color
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
  grabPc (view piece target)
  placeShip loc $ Ship player (view piece target)

tradeShip ∷ SystemId → Piece → Color → HWMove ()
tradeShip loc pc newColor = do
  player ← use playerToMv
  let oldShip = Ship player pc
  guard =<< actionAvailable loc Blue
  guard =<< shipExistsAt loc oldShip
  let newShip = set (piece . color) newColor oldShip
  deleteShip loc oldShip
  grabPc (newShip ^. piece)
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
  to ← grabOrLookupDestination dest
  guard =<< canMoveTo loc to
  let ship = Ship player pc
  deleteShip loc ship
  grabPc (ship ^. piece)
  placeShip to ship

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
    []  → destroySystem loc
    [p] → modify $ over systems
                 $ flip IntMap.update loc
                 $ Just . set star (Single p)

causeCatastrophe ∷ SystemId → Color → HWMove ()
causeCatastrophe loc c = do
  fuel ← piecesAt loc (\p → c == p^.color)
  guard $ length fuel >= 4

  -- It's important that we do this before we kill the star, otherwise we
  -- might try to delete ships that no longer exist.
  targets ← shipsAt loc (\p → c == p^.piece.color)
  forM_ targets (deleteShip loc)

  deathstar ← (c `elem`) <$> systemStarColors loc
  when deathstar (destroyStarsWithColor loc c)

cleanup ∷ HWMove ()
cleanup = do
  sysIds ← IntMap.keys . view systems <$> get
  forM_ sysIds $ \loc → do
    docked ← view ships <$> lookupSystem loc
    when (null docked) $ destroySystem loc

applyAction ∷ Action → HWMove ()
applyAction act = do
  act & \case
    Construct   loc c        → constructShip loc c
    Move        loc p dest   → moveShip loc p dest
    Attack      loc target   → attackShip loc target
    Trade       loc target c → tradeShip loc target c
    Sacrifice   loc target   → sacrificeShip loc target
    Catastrophe loc color    → causeCatastrophe loc color
  cleanup

applyEvent ∷ Event → HWMove Event
applyEvent ev = do
  ev & \case
    Join setup   → void (joinGame setup)
    Resign       → undefined
    Turn actions → do guard $ validActionSequence actions
                      sequence_ (applyAction <$> actions)
                      advanceTurn
  return ev
