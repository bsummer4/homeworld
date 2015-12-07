{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE TupleSections       #-}

module Main where

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


-- General Utilities -----------------------------------------------------------------------------------------

safeHead ∷ [a] → Maybe a
safeHead = \case { [] → Nothing; x:_ → Just x }


-- Types -----------------------------------------------------------------------------------------------------

data Color = Red | Green | Blue | Yellow
  deriving (Eq, Ord, Enum, Bounded)

data Size = Small | Medium | Large
  deriving (Eq, Ord, Enum, Bounded)

data Piece = Piece { _color ∷ Color, _size ∷ Size }
  deriving (Eq, Ord)

data Ship = Ship { _owner ∷ PlayerId, _piece ∷ Piece }
  deriving (Eq, Ord)

data Star = Binary Piece Piece
          | Single Piece
  deriving (Eq, Ord, Show)

data System = System { _star ∷ Star, _ships ∷ [Ship] }
  deriving (Eq, Ord, Show)

type PlayerId = Int
type SystemId = Int

data GameSt = GameSt { _reserve    ∷ Map Piece Int
                     , _systems    ∷ Map SystemId System
                     , _turn       ∷ PlayerId
                     , _numSystems ∷ Int
                     , _numPlayers ∷ Int
                     , _history    ∷ [Event]
                     }
  deriving (Eq, Ord, Show)


-- Moves -----------------------------------------------------------------------------------------------------

data Setup = Setup Piece Piece Color
  deriving (Eq, Ord, Show)

data Destination = Existing SystemId
                 | Fresh Piece
  deriving (Eq, Ord, Show)

data Action = Construct SystemId Color
            | Trade SystemId Piece Color
            | Move SystemId Piece Destination
            | Attack SystemId Ship
            | Sacrifice SystemId Piece
            | Catastrophe SystemId Color
  deriving (Eq, Ord, Show)

data Event = Join Setup
           | Resign
           | Turn [Action]
  deriving (Eq, Ord, Show)


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

exampleGame =
  [ Join (Setup (Piece Red Small) (Piece Green Medium) Blue)
  , Join (Setup (Piece Red Small) (Piece Green Medium) Yellow)

  , Turn [Construct 0 Blue]
  , Turn [Construct 1 Yellow]

  , Turn [Trade 0 (Piece Blue Small) Yellow]
  , Turn [Move 0 (Piece Yellow Large) (Fresh (Piece Red Large))]

  , Turn [Move 0 (Piece Yellow Small) (Existing 2)]
  , Turn [Attack 2 (Ship 0 (Piece Yellow Small))]

  , Turn [ Catastrophe 99 Blue
         , Catastrophe 99 Green
         , Attack 2 (Ship 0 (Piece Yellow Small))
         , Catastrophe 99 Blue
         , Catastrophe 99 Green
         ]

  , Turn [ Catastrophe 99 Yellow
         , Sacrifice 234 (Piece Green Small)
         , Attack 2 (Ship 0 (Piece Yellow Small))
         , Catastrophe 99 Yellow
         ]
  ]


-- Utilities -------------------------------------------------------------------------------------------------

enumeration ∷ (Bounded a, Enum a) ⇒ [a]
enumeration = [minBound .. maxBound]


-- The Game Monad --------------------------------------------------------------------------------------------

type Game = StateT GameSt []

runGame ∷ Game a → [(a, GameSt)]
runGame = flip runStateT emptyState

execGame ∷ Game a → [GameSt]
execGame = flip execStateT emptyState

evalGame ∷ Game a → [a]
evalGame = flip evalStateT emptyState


-- Initial States --------------------------------------------------------------------------------------------

-- This is three rainbow stashes.
startingDeck ∷ Map Piece Int
startingDeck = Map.fromList $ do
  color <- enumeration
  size <- enumeration
  return (Piece color size, 9)

emptyState ∷ GameSt
emptyState = GameSt
  { _reserve    = startingDeck
  , _systems    = Map.empty
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

currentPlayer ∷ Game PlayerId
currentPlayer = view turn <$> get

pieceAvailable ∷ Piece → Game Bool
pieceAvailable pc = do
  st ← get
  let inReserve = fromMaybe 0 $ Map.lookup pc $ st ^. reserve
  return (inReserve > 0)

smallestAvailableSizeFor ∷ Color → Game Size
smallestAvailableSizeFor c = do
  sizeAvailability ← mapM (\sz → (sz,) <$> pieceAvailable (Piece c sz)) [Small ..]
  let sizesAvailable = fst <$> filter snd sizeAvailability
  lift $ toList $ safeHead sizesAvailable

takeSmallestShipFromReserve ∷ Color → Game Size
takeSmallestShipFromReserve color = do
  sz ← smallestAvailableSizeFor color
  takePc (Piece color sz)
  return sz


-- Setup -----------------------------------------------------------------------------------------------------

createSystem ∷ System → Game SystemId
createSystem sys@(System star ships) = do
  case star of Single s     → takePc s
               Binary s1 s2 → takePc s1 >> takePc s2
  mapM_ takePc (fmap (view piece) ships)
  st ← get
  let newId = st ^. numSystems
  modify $ set numSystems (newId + 1)
  modify $ set systems (Map.insert newId sys (st ^. systems))
  return newId

joinGame ∷ Setup → Game PlayerId
joinGame (Setup s1 s2 c) = do
  st ← get
  let playerId = st ^. numPlayers
  systemId ← createSystem $ System (Binary s1 s2) [Ship playerId (Piece c Large)]
  modify $ set numPlayers ((st ^. numPlayers) + 1)
  return playerId

initialize ∷ [Setup] → Game ()
initialize = mapM_ joinGame


-- Game Logic Basics -----------------------------------------------------------------------------------------

getSystem ∷ SystemId → Game System
getSystem sysId = do
  st ← get
  lift $ toList $ Map.lookup sysId $ view systems st

destinationId ∷ Destination → Game SystemId
destinationId (Existing loc) = return loc
destinationId (Fresh pc)     = do
  guard =<< pieceInReserve pc
  createSystem (System (Single pc) [])

shipsAt ∷ SystemId → (Ship → Bool) → Game [Ship]
shipsAt loc pred = filter pred . view ships <$> getSystem loc

piecesAt ∷ SystemId → (Piece → Bool) → Game [Piece]
piecesAt loc pred = do
  sys ← getSystem loc
  return $ filter pred
         $ fmap (view piece) (view ships sys)
        <> starMakeup (sys^.star)

shipsExistAt ∷ SystemId → (Ship → Bool) → Game Bool
shipsExistAt loc  pred = not . null <$> shipsAt loc pred

shipExistsAt ∷ SystemId → Ship → Game Bool
shipExistsAt loc ship = shipsExistAt loc (== ship)

shipSize ∷ Ship → Size
shipSize = view (piece . size)

biggestShip ∷ [Ship] → (Maybe Ship)
biggestShip = safeHead . reverse . List.sortOn shipSize

ourLargestShipAt ∷ SystemId → Game Ship
ourLargestShipAt loc = do
  us ← currentPlayer
  mShip ← biggestShip <$> shipsAt loc (\ship → us == view owner ship)
  lift $ toList mShip

ownsAShipWith ∷ SystemId → PlayerId → Color → Game Bool
ownsAShipWith loc player color =
  shipsExistAt loc $ \(Ship owner (Piece c _)) →
    and [owner == player, c == color]

homeWorld ∷ PlayerId → Game System
homeWorld = getSystem

advanceTurn ∷ Game ()
advanceTurn =
  modify $ \st → set turn ((view turn st + 1) `mod` (view numPlayers st)) st

placeShip ∷ SystemId → Ship → Game ()
placeShip loc p = modify $ \st →
  st & systems .~ Map.update f loc (st ^. systems)
    where
      f sys = Just $ over ships (p:) sys

pieceInReserve ∷ Piece → Game Bool
pieceInReserve pc = do
  st ← get
  inReserve ← lift $ toList $ Map.lookup pc (st ^. reserve)
  return $ inReserve >= 1

takePc ∷ Piece → Game ()
takePc pc = do
  guard =<< pieceInReserve pc
  modify $ over reserve $ flip Map.update pc $ Just . (+1)

deleteShip ∷ SystemId → Ship → Game ()
deleteShip loc ship = do
  guard =<< shipExistsAt loc ship
  modify $ over systems
         $ flip Map.update loc
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

logEvent ∷ Event → Game ()
logEvent e = modify $ over history (e:)

actionAvailable ∷ SystemId → Color → Game Bool
actionAvailable loc col = do
  sys ← getSystem loc
  return $ col `elem` colorsAvailable sys

noneEqual ∷ Eq a ⇒ [a] → Bool
noneEqual [] = True
noneEqual (x:xs) = all (/= x) xs && noneEqual xs

canTeleportBetween ∷ System → System → Bool
canTeleportBetween sys1 sys2 =
  noneEqual $ map (view size) $ concat $ map (starMakeup . view star) [sys1,sys2]

canMoveTo ∷ SystemId → SystemId → Game Bool
canMoveTo from to =
  if from == to
    then return False
    else canTeleportBetween <$> getSystem from <*> getSystem to

constructShip ∷ SystemId → Color → Game ()
constructShip loc color = do
  player ← currentPlayer
  guard =<< ownsAShipWith loc player color
  guard =<< actionAvailable loc color
  size ← takeSmallestShipFromReserve color
  placeShip loc $ Ship player $ Piece color size

attackShip ∷ SystemId → Ship → Game ()
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

tradeShip ∷ SystemId → Piece → Color → Game ()
tradeShip loc pc newColor = do
  player ← currentPlayer
  let oldShip = Ship player pc
  guard =<< actionAvailable loc Blue
  guard =<< shipExistsAt loc oldShip
  let newShip = set (piece . color) newColor oldShip
  deleteShip loc oldShip
  takePc (newShip ^. piece)
  placeShip loc newShip

sacrificeShip ∷ SystemId → Piece → Game ()
sacrificeShip loc pc = do
  player ← currentPlayer
  let ship = Ship player pc
  guard =<< shipExistsAt loc ship
  deleteShip loc ship

moveShip ∷ SystemId → Piece → Destination → Game ()
moveShip loc pc dest = do
  player ← currentPlayer
  guard =<< actionAvailable loc Yellow
  to ← destinationId dest
  guard =<< canMoveTo loc to
  let ship = Ship player pc
  deleteShip loc ship
  takePc (ship ^. piece)
  placeShip to ship

systemStarColors ∷ SystemId → Game [Color]
systemStarColors loc = do
  sys ← getSystem loc
  return $ view color <$> starMakeup (view star sys)

destroySystem ∷ SystemId → Game ()
destroySystem = modify . over systems . Map.delete

destroyStarsWithColor ∷ SystemId → Color → Game ()
destroyStarsWithColor loc c = do
  thisStar ← view star <$> getSystem loc

  let starPieces = starMakeup thisStar
      remaining  = filter (\p → p^.color /= c) starPieces

  guard $ length starPieces /= length remaining
  case remaining of
    []  → destroySystem loc
    [p] → modify $ over systems
                 $ flip Map.update loc
                 $ Just . set star (Single p)

causeCatastrophe ∷ SystemId → Color → Game ()
causeCatastrophe loc c = do
  fuel ← piecesAt loc (\p → c == p^.color)
  guard $ length fuel >= 4

  -- It's important that we do this before we kill the star, otherwise we
  -- might try to delete ships that no longer exist.
  targets ← shipsAt loc (\p → c == p^.piece.color)
  forM_ targets (deleteShip loc)

  deathstar ← (c `elem`) <$> systemStarColors loc
  when deathstar (destroyStarsWithColor loc c)

applyAction ∷ Action → Game ()
applyAction = \case
  Construct   loc c        → constructShip loc c
  Move        loc p dest   → moveShip loc p dest
  Attack      loc target   → attackShip loc target
  Trade       loc target c → tradeShip loc target c
  Sacrifice   loc target   → sacrificeShip loc target
  Catastrophe loc color    → causeCatastrophe loc color

applyEvent ∷ Event → Game ()
applyEvent ev = do
  ev & \case
    Join setup   → void (joinGame setup)
    Resign       → undefined
    Turn actions → do guard $ validActionSequence actions
                      sequence_ (applyAction <$> actions)
                      advanceTurn
  logEvent ev


-- Arb Stuff -------------------------------------------------------------------------------------------------

arbitrarySystem ∷ Game SystemId
arbitrarySystem = do
  numSystems ← view numSystems <$> get
  lift [0 .. numSystems-1]

arbitraryPiece ∷ Game Piece
arbitraryPiece = Piece <$> lift enumeration <*> lift enumeration

arbitraryDestination ∷ Game Destination
arbitraryDestination = do
  freshDest ← lift [True, False]
  if freshDest then Fresh    <$> arbitraryPiece
               else Existing <$> arbitrarySystem

arbitraryMove ∷ Game Action
arbitraryMove = do
  loc  ← arbitrarySystem
  pl   ← currentPlayer
  sys  ← getSystem loc
  pc   ← lift $ map (view piece) $ filter ((pl ==) . view owner) $ view ships sys
  dest ← arbitraryDestination
  return $ Move loc pc dest

arbitraryAttack ∷ Game Action
arbitraryAttack = do
  loc    ← arbitrarySystem
  pl     ← currentPlayer
  sys    ← getSystem loc
  target ← lift $ filter ((pl /=) . view owner) $ view ships sys
  return $ Attack loc target

arbitraryTrade ∷ Game Action
arbitraryTrade = do
  loc    ← arbitrarySystem
  sys    ← getSystem loc
  pl     ← currentPlayer
  target ← lift $ filter ((pl ==) . view owner) $ view ships sys
  color  ← lift $ filter (/= target^.piece.color) enumeration
  return $ Trade loc (target ^. piece) color

arbitraryConstruction ∷ Game Action
arbitraryConstruction = Construct <$> arbitrarySystem <*> lift enumeration

arbitraryCatastrophe ∷ Game Action
arbitraryCatastrophe = do
  loc     ← arbitrarySystem
  c       ← lift enumeration
  targets ← piecesAt loc (\p → c == p^.color)
  guard $ length targets >= 4
  return (Catastrophe loc c)

possibleCatastrophes ∷ Game [Action]
possibleCatastrophes = evalStateT arbitraryCatastrophe <$> get

arbitrarySacrifice ∷ Game Action
arbitrarySacrifice = do
  loc    ← arbitrarySystem
  sys    ← getSystem loc
  pl     ← currentPlayer
  target ← lift $ filter ((pl ==) . view owner) $ view ships sys
  return $ Sacrifice loc (target ^. piece)

arbitraryActionSequence ∷ Game [Action]
arbitraryActionSequence = do
  action ← join $ lift [arbitraryAction, arbitrarySacrifice]
  case action of
    Sacrifice loc pc → fmap (action:) $ replicateM (piecePower pc) $ case (view color pc) of
                           Red    → arbitraryAttack
                           Green  → arbitraryConstruction
                           Blue   → arbitraryTrade
                           Yellow → arbitraryMove
    _                → return [action]

arbitraryActionSequenceOfDeath ∷ Game [Action]
arbitraryActionSequenceOfDeath =
  possibleCatastrophes >>= \case
    []   → arbitraryActionSequence
    evil → (evil ++) <$> arbitraryActionSequence

arbitraryAction ∷ Game Action
arbitraryAction = join $ lift [ arbitraryMove
                              , arbitraryConstruction
                              , arbitraryAttack
                              , arbitraryTrade
                              ]

arbitrarySetup ∷ Game Setup
arbitrarySetup = do
  p1 ← arbitraryPiece
  p2 ← arbitraryPiece
  guard $ p1 >= p2
  c ← lift enumeration

  -- Just to constrain the space
  guard $ p1^.size /= p2^.size
  guard $ p1^.color /= p2^.color
  guard $ c         /= p2^.color
  guard $ p1^.color /= c

  return (Setup p1 p2 c)


-- Execute Arbitrary Actions ---------------------------------------------------------------------------------

performArbitraryJoin ∷ Game ()
performArbitraryJoin = do
  setup ← arbitrarySetup
  applyEvent (Join setup)

performArbitraryActionSequence ∷ Game ()
performArbitraryActionSequence = do
  actions ← arbitraryActionSequenceOfDeath
  applyEvent (Turn actions)


-- Tie everything together. ----------------------------------------------------------------------------------

ordNub ∷ Ord a ⇒ [a] → [a]
ordNub = Set.toList . Set.fromList

executeEventLog ∷ [Event] → IO ()
executeEventLog = loop emptyState
  where
    loop st events = do
      putStrLn $ ppShow st
      putStrLn ""
      events & \case
        []     → putStrLn "done"
        (e:es) → do
          putStrLn $ ppShow e
          putStrLn ""
          execStateT (applyEvent e) st & \case
            []    → putStrLn "Invalid move! aborting."
            _:_:_ → putStrLn "Ambiguous move! aborting."
            [st'] → loop st' es

main ∷ IO ()
main = do
  LBS8.putStrLn $ A.encode exampleGame

  let allInitialStates = execGame $ do
        performArbitraryJoin
        performArbitraryJoin

  initial ← head <$> shuffleM (take 100 allInitialStates)

  putStrLn "INITIAL STATE"
  putStrLn $ ppShow initial

  let allFollowupStates = flip execStateT initial $ replicateM 4 performArbitraryActionSequence

  print (length allFollowupStates)

  someGames ← take 100 <$> shuffleM allFollowupStates

  mapM_ (\x → print x >> putStrLn "") $ reverse <$> view history <$> someGames

  executeEventLog $ reverse $ view history $ head someGames
