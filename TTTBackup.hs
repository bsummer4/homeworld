module TTT where

import GamePrelude

import           Control.Monad.State.Lazy
import           Data.Foldable            (length, toList)
import           Data.List                (foldl')
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Maybe
import           Data.Tree                as Tree
import           Data.Tree                (Tree, Forest)
import           System.Random.Shuffle    (shuffleM)


-- Types -----------------------------------------------------------------------------------------------------

data Player = X | O
  deriving (Eq,Ord,Show,Enum,Bounded)

data Event = Place !Int !Int
  deriving (Eq, Ord, Show)

type GameSt = (Player, Map (Int,Int) Player)


-- Initial State ---------------------------------------------------------------------------------------------

initialState = (X, Map.empty)


-- The Game Monad --------------------------------------------------------------------------------------------

type Game = StateT GameSt []

runGame  ∷ Game a → [(a, GameSt)]
runGame  = flip runStateT initialState

execGame ∷ Game a → [GameSt]
execGame = flip execStateT initialState

evalGame ∷ Game a → [a]
evalGame = flip evalStateT initialState


-- Game Logic ------------------------------------------------------------------------------------------------

winningRows ∷ [[(Int,Int)]]
winningRows = [ [(0,0), (1,0), (2,0)]
              , [(0,1), (1,1), (2,1)]
              , [(0,2), (1,2), (2,2)]
              , [(0,0), (0,1), (0,2)]
              , [(1,0), (1,1), (1,2)]
              , [(2,0), (2,1), (2,2)]
              , [(0,0), (1,1), (2,2)]
              , [(0,2), (1,1), (2,0)]
              ]

winnerOnRow ∷ GameSt → [(Int,Int)] → Maybe Player
winnerOnRow (_,board) idxs =
    case (\k → Map.lookup k board) <$> idxs of
      vals@(Just pl:_) → if (allEqual vals) then (Just pl) else Nothing
      _                → Nothing

winner ∷ GameSt → Maybe Player
winner st = safeHead $ catMaybes $ winnerOnRow st <$> winningRows

assertNoWinner ∷ Game ()
assertNoWinner = do
  st ← get
  guard (isNothing (winner st))

flipTurn ∷ Game ()
flipTurn = modify $ \case
  (X,b) → (O,b)
  (O,b) → (X,b)

writeSquare ∷ (Int,Int) → Player → Game ()
writeSquare k v = do
  (t,b) ← get
  guard $ isNothing $ Map.lookup k b
  put (t, Map.insert k v b)

assertValidIdx ∷ (Int, Int) → Game ()
assertValidIdx (x,y) = guard (and [x>=0, x<=2, y>=0, y<=2])

activePlayer ∷ Game Player
activePlayer = fst <$> get

place ∷ (Int, Int) → Game ()
place idx = do
  assertValidIdx idx
  c ← activePlayer
  writeSquare idx c
  flipTurn

arbitraryMove ∷ Game Event
arbitraryMove = do
  assertNoWinner
  x ← lift [0..2]
  y ← lift [0..2]
  let e = Place x y
  place (x,y)
  return e


-- Operations on Game Trees ----------------------------------------------------------------------------------

data GameTree e s     = GameTree s (Forest (e,s)) deriving (Eq,Show,Foldable)
data GameTrace e s    = Trace s [(e,s)]           deriving (Eq,Ord,Show)
type Actor e s        = GameTree e s -> IO (Maybe (e, GameTree e s))
type Scoring s        = s -> Int

playGame ∷  Actor e s → GameTree e s → IO (GameTrace e s)
playGame actor initial@(GameTree initialState _) = Trace initialState <$> loop initial
  where
    loop tr = do
      actor tr >>= \case
        Nothing → return []
        Just (e, subtree@(GameTree st' _)) → ((e, st'):) <$> loop subtree

randomActor ∷ Actor e s
randomActor (GameTree st elems) = fmap f . safeHead <$> shuffleM elems
  where
    f (Tree.Node (e, st') forest) = (e, GameTree st' forest)

randomGame ∷ GameTree e s -> IO (GameTrace e s)
randomGame = playGame randomActor

interleaveActs ∷ [Actor e s] -> IO (Actor e s)
interleaveActs actors = undefined

humanActor ∷ (Show e, Show s) => Actor e s
humanActor = undefined

naiveAI ∷ Scoring s -> Actor e s
naiveAI = undefined

lookaheadAI ∷ Int -> Scoring s -> Actor e s
lookaheadAI = undefined

-- Tie it all together ---------------------------------------------------------------------------------------

move ∷ GameSt → Game a → [GameSt]
move st action = execStateT action st

stateAfter ∷ Game a → [GameSt]
stateAfter = move initialState

edgesFrom ∷ GameSt → Forest (Event, GameSt)
edgesFrom st = f <$> runStateT arbitraryMove st
  where
    f (e,st') = Tree.Node (e,st') (edgesFrom st')

gameTreeFrom ∷ GameSt → GameTree Event GameSt
gameTreeFrom st = GameTree st (edgesFrom st)

gameTree ∷ GameTree Event GameSt
gameTree = gameTreeFrom initialState

numberOfReachableGameStates = length gameTree

reachableStatesWherePlayerWinsAfterThis player =
  filter (\st → Just player == winner st) . toList . gameTreeFrom

allPossibleStatesAtDepth ∷ Int → [GameSt]
allPossibleStatesAtDepth depth = execGame (replicateM depth arbitraryMove)

[someArbitraryState] = stateAfter $ do
    place (1,1)
    place (0,0)
    place (1,0)

main = do
    randomGame gameTree >>= \case
        Trace initialSt history → do
            print initialSt
            forM_ history $ \(e,st) → print e >> print st

    when False $ do
      print $ (length . allPossibleStatesAtDepth) <$> [0..9]
      print $ (length . allPossibleStatesAtDepth) <$> [0..9]

      print $ length $ reachableStatesWherePlayerWinsAfterThis X initialState

      print $ length $ reachableStatesWherePlayerWinsAfterThis X someArbitraryState

      print $ length $ reachableStatesWherePlayerWinsAfterThis O someArbitraryState
