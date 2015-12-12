module Moves where

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

import           THUtils

import Types


-- General Utilities -----------------------------------------------------------------------------------------

enumeration ∷ (Bounded a, Enum a) ⇒ [a]
enumeration = [minBound .. maxBound]

safeHead ∷ [a] → Maybe a
safeHead = \case { [] → Nothing; x:_ → Just x }


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
  , _turn       = 0
  , _numSystems = 0
  , _numPlayers = 0
  , _history    = []
  }


-- Questions about the game state ----------------------------------------------------------------------------

starMakeup ∷ Star → [Piece]
starMakeup = \case { Single x → [x]; Binary x y → [x,y] }

colorsAvailable ∷ System → [Color]
colorsAvailable system = (map (view color) $ starMakeup $ view star system)
                      ++ (map (view (piece . color)) $ view ships system)

currentPlayer ∷ MoveM PlayerId
currentPlayer = view turn <$> get

pieceAvailable ∷ Piece → MoveM Bool
pieceAvailable pc = do
  st ← get
  let inReserve = fromMaybe 0 $ IntMap.lookup (fromEnum pc) $ st ^. reserve
  return (inReserve > 0)

smallestAvailableSizeFor ∷ Color → MoveM Size
smallestAvailableSizeFor c = do
  sizeAvailability ← mapM (\sz → (sz,) <$> pieceAvailable (Piece c sz)) [Small ..]
  let sizesAvailable = fst <$> filter snd sizeAvailability
  lift $ safeHead sizesAvailable

takeSmallestShipFromReserve ∷ Color → MoveM Size
takeSmallestShipFromReserve color = do
  sz ← smallestAvailableSizeFor color
  takePc (Piece color sz)
  return sz


-- Setup -----------------------------------------------------------------------------------------------------

createSystem ∷ System → MoveM SystemId
createSystem sys@(System star ships) = do
  case star of Single s     → takePc s
               Binary s1 s2 → takePc s1 >> takePc s2
  mapM_ takePc (fmap (view piece) ships)
  st ← get
  let newId = st ^. numSystems
  modify $ set numSystems (newId + 1)
  modify $ set systems (IntMap.insert newId sys (st ^. systems))
  return newId

joinGame ∷ Setup → MoveM PlayerId
joinGame (Setup s1 s2 c) = do
  st ← get
  let playerId = st ^. numPlayers
  systemId ← createSystem $ System (Binary s1 s2) [Ship playerId (Piece c Large)]
  modify $ set numPlayers ((st ^. numPlayers) + 1)
  return playerId

initialize ∷ [Setup] → MoveM ()
initialize = mapM_ joinGame


-- Game Logic Basics -----------------------------------------------------------------------------------------

getSystem ∷ SystemId → MoveM System
getSystem sysId = do
  st ← get
  lift $ IntMap.lookup sysId $ view systems st

destinationId ∷ Destination → MoveM SystemId
destinationId (Existing loc) = return loc
destinationId (Fresh pc)     = do
  guard =<< pieceInReserve pc
  createSystem (System (Single pc) [])

shipsAt ∷ SystemId → (Ship → Bool) → MoveM [Ship]
shipsAt loc pred = filter pred . view ships <$> getSystem loc

piecesAt ∷ SystemId → (Piece → Bool) → MoveM [Piece]
piecesAt loc pred = do
  sys ← getSystem loc
  return $ filter pred
         $ fmap (view piece) (view ships sys)
        <> starMakeup (sys^.star)

shipsExistAt ∷ SystemId → (Ship → Bool) → MoveM Bool
shipsExistAt loc  pred = not . null <$> shipsAt loc pred

shipExistsAt ∷ SystemId → Ship → MoveM Bool
shipExistsAt loc ship = shipsExistAt loc (== ship)

shipSize ∷ Ship → Size
shipSize = view (piece . size)

biggestShip ∷ [Ship] → (Maybe Ship)
biggestShip = safeHead . reverse . List.sortOn shipSize

ourLargestShipAt ∷ SystemId → MoveM Ship
ourLargestShipAt loc = do
  us ← currentPlayer
  mShip ← biggestShip <$> shipsAt loc (\ship → us == view owner ship)
  lift mShip

ownsAShipWith ∷ SystemId → PlayerId → Color → MoveM Bool
ownsAShipWith loc player color =
  shipsExistAt loc $ \(Ship owner (Piece c _)) →
    and [owner == player, c == color]

homeWorld ∷ PlayerId → MoveM System
homeWorld = getSystem

advanceTurn ∷ MoveM ()
advanceTurn =
  modify $ \st → set turn ((view turn st + 1) `mod` (view numPlayers st)) st

placeShip ∷ SystemId → Ship → MoveM ()
placeShip loc p = modify $ \st →
  st & systems .~ IntMap.update f loc (st ^. systems)
    where
      f sys = Just $ over ships (p:) sys

pieceInReserve ∷ Piece → MoveM Bool
pieceInReserve pc = do
  st ← get
  inReserve ← lift $ IntMap.lookup (fromEnum pc) (st ^. reserve)
  return $ inReserve >= 1

takePc ∷ Piece → MoveM ()
takePc pc = do
  guard =<< pieceInReserve pc
  modify $ over reserve $ flip IntMap.update (fromEnum pc) $ Just . (+1)

deleteShip ∷ SystemId → Ship → MoveM ()
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

logEvent ∷ Event → MoveM ()
logEvent e = modify $ over history (e:)

actionAvailable ∷ SystemId → Color → MoveM Bool
actionAvailable loc col = do
  sys ← getSystem loc
  return $ col `elem` colorsAvailable sys

noneEqual ∷ Eq a ⇒ [a] → Bool
noneEqual [] = True
noneEqual (x:xs) = all (/= x) xs && noneEqual xs

canTeleportBetween ∷ System → System → Bool
canTeleportBetween sys1 sys2 =
  noneEqual $ map (view size) $ concat $ map (starMakeup . view star) [sys1,sys2]

canMoveTo ∷ SystemId → SystemId → MoveM Bool
canMoveTo from to =
  if from == to
    then return False
    else canTeleportBetween <$> getSystem from <*> getSystem to

constructShip ∷ SystemId → Color → MoveM ()
constructShip loc color = do
  player ← currentPlayer
  guard =<< ownsAShipWith loc player color
  guard =<< actionAvailable loc color
  size ← takeSmallestShipFromReserve color
  placeShip loc $ Ship player $ Piece color size

attackShip ∷ SystemId → Ship → MoveM ()
attackShip loc target = do
  player ← currentPlayer
  guard =<< actionAvailable loc Red
  guard =<< shipExistsAt loc target
  guard $ view owner target /= player

  attacker ← ourLargestShipAt loc
  guard $ target^.piece.size <= attacker^.piece.size

  deleteShip loc target
  takePc (view piece target)
  placeShip loc $ Ship player (view piece target)

tradeShip ∷ SystemId → Piece → Color → MoveM ()
tradeShip loc pc newColor = do
  player ← currentPlayer
  let oldShip = Ship player pc
  guard =<< actionAvailable loc Blue
  guard =<< shipExistsAt loc oldShip
  let newShip = set (piece . color) newColor oldShip
  deleteShip loc oldShip
  takePc (newShip ^. piece)
  placeShip loc newShip

sacrificeShip ∷ SystemId → Piece → MoveM ()
sacrificeShip loc pc = do
  player ← currentPlayer
  let ship = Ship player pc
  guard =<< shipExistsAt loc ship
  deleteShip loc ship

moveShip ∷ SystemId → Piece → Destination → MoveM ()
moveShip loc pc dest = do
  player ← currentPlayer
  guard =<< actionAvailable loc Yellow
  to ← destinationId dest
  guard =<< canMoveTo loc to
  let ship = Ship player pc
  deleteShip loc ship
  takePc (ship ^. piece)
  placeShip to ship

systemStarColors ∷ SystemId → MoveM [Color]
systemStarColors loc = do
  sys ← getSystem loc
  return $ view color <$> starMakeup (view star sys)

-- TODO Add everything back to the bank.
destroySystem ∷ SystemId → MoveM ()
destroySystem = modify . over systems . IntMap.delete

destroyStarsWithColor ∷ SystemId → Color → MoveM ()
destroyStarsWithColor loc c = do
  thisStar ← view star <$> getSystem loc

  let starPieces = starMakeup thisStar
      remaining  = filter (\p → p^.color /= c) starPieces

  guard $ length starPieces /= length remaining
  case remaining of
    []  → destroySystem loc
    [p] → modify $ over systems
                 $ flip IntMap.update loc
                 $ Just . set star (Single p)

causeCatastrophe ∷ SystemId → Color → MoveM ()
causeCatastrophe loc c = do
  fuel ← piecesAt loc (\p → c == p^.color)
  guard $ length fuel >= 4

  -- It's important that we do this before we kill the star, otherwise we
  -- might try to delete ships that no longer exist.
  targets ← shipsAt loc (\p → c == p^.piece.color)
  forM_ targets (deleteShip loc)

  deathstar ← (c `elem`) <$> systemStarColors loc
  when deathstar (destroyStarsWithColor loc c)

cleanup ∷ MoveM ()
cleanup = do
  sysIds ← IntMap.keys . view systems <$> get
  forM_ sysIds $ \loc → do
    docked ← view ships <$> getSystem loc
    when (null docked) $ destroySystem loc

applyAction ∷ Action → MoveM ()
applyAction act = do
  act & \case
    Construct   loc c        → constructShip loc c
    Move        loc p dest   → moveShip loc p dest
    Attack      loc target   → attackShip loc target
    Trade       loc target c → tradeShip loc target c
    Sacrifice   loc target   → sacrificeShip loc target
    Catastrophe loc color    → causeCatastrophe loc color
  cleanup

applyEvent ∷ Event → MoveM ()
applyEvent ev = do
  ev & \case
    Join setup   → void (joinGame setup)
    Resign       → undefined
    Turn actions → do guard $ validActionSequence actions
                      sequence_ (applyAction <$> actions)
                      advanceTurn
  logEvent ev
