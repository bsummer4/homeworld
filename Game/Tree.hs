module Game.Tree where

import Core hiding (below, children, init, pred)

import Control.Monad.State.Lazy
import Data.Tree                as Tree
import System.Random.Shuffle    (shuffleM)



-- Types -----------------------------------------------------------------------------------------------------

-- | This describes a single, possibly-invalid move. Given a state, either
-- return `Nothing` or return an event and it's associated state.
type Move st = StateT st Maybe

-- | This describes a game. Given a state, return a list of all possible
-- follow-up events and their associated states.
type Game st = StateT st []

type GameSubTree e st  = Tree (e,st)
data GameTree e st     = GameTree st [GameSubTree e st] deriving (Eq,Show,Functor,Foldable,Traversable)
data GameTrace e st    = GameTrace st [(e,st)]          deriving (Eq,Ord,Show,Functor,Foldable,Traversable)
type Actor m e st      = GameTree e st -> m (Maybe (GameSubTree e st))
type ActorIO e st      = Actor IO e st
type Scoring st        = st -> Int


-- Operations on Games and Moves -----------------------------------------------------------------------------

edgesFrom ∷ st → Game st e → [(e, st)]
edgesFrom st game = runStateT game st

treesFrom ∷ st → Game st e → Forest (e, st)
treesFrom init game = f <$> edgesFrom init game
  where f (e,st) = Node (e,st) (treesFrom st game)

gameTree ∷ st → Game st e → GameTree e st
gameTree init game = GameTree init $ treesFrom init game


-- Operations on GameTrees -----------------------------------------------------------------------------------

stateTree ∷ GameTree e st → Tree st
stateTree (GameTree init moves) = Node init $ fmap snd <$> moves

eventForests ∷ GameTree e st → Forest e
eventForests (GameTree _ moves) = fmap fst <$> moves

gameStates ∷ GameTree e st → [st]
gameStates = toList . stateTree

stateSpace ∷ GameTree e st → Int
stateSpace = length . gameStates

reachableStatesWhere ∷ (st → Bool) → GameTree e st → [st]
reachableStatesWhere pred = filter pred . gameStates

statesAtDepth ∷ Int → GameTree e st → [st]
statesAtDepth depth = fromMaybe [] . safeHead . drop depth . Tree.levels . stateTree


-- Actors ----------------------------------------------------------------------------------------------------

-- GameTree e st -> m (Maybe (e, GameTree e st))

dumbActor ∷ Monad m => Actor m e s
dumbActor (GameTree _ elems) = safeHead <$> return elems

randomActor ∷ ActorIO e s
randomActor (GameTree _ elems) = safeHead <$> shuffleM (take 100 elems)

interleaveActs ∷ [Actor m e st] -> IO (Actor m e st)
interleaveActs _actors = undefined

-- type Actor m e st      = GameTree e st -> m (Maybe (GameSubTree e st))
humanActor ∷ (Show e, Show st) => ActorIO e st
humanActor (GameTree initialState []) = do
    putStrLn "The game is over!"
    putStrLn "Final state:"
    pprint initialState
    return Nothing

humanActor (GameTree initialState possibilities) = do
    putStrLn "=========== Current Game State ==============="
    pprint initialState

    let possibleEvents = fst . Tree.rootLabel <$> possibilities
    forM_ (zip [0∷Int ..] possibleEvents) $ \ (i,e) → do
      putStrLn ("  " <> cs(show i) <> ". " <> cs(show e))
    let numPossibilities = length possibilities
    n ← read <$> getLine
    let choice = possibilities !! (n `mod` numPossibilities)
    let choiceEv = possibleEvents !! (n `mod` numPossibilities)
    putStrLn ("You chose this: " <> cs(show choiceEv))
    putStrLn ("Push enter to continue")
    _ ← getLine
    return (Just choice)

naiveAI ∷ Scoring st -> Actor m e st
naiveAI = undefined

lookaheadAI ∷ Int -> Scoring st -> Actor m e st
lookaheadAI = undefined


-- Operations on Traces --------------------------------------------------------------------------------------

traceEvents ∷ GameTrace e st → [e]
traceEvents (GameTrace _ hist) = fst <$> hist

printTraceHistory ∷ (Show e, Show st) ⇒ GameTrace e st → IO ()
printTraceHistory tr@(GameTrace init _) = do
  print init
  forM_ (traceEvents tr) $ \e → do
    print e

printTrace ∷ (Show e, Show a) ⇒ (st → a) → GameTrace e st → IO ()
printTrace disp (GameTrace init happenings) = do
  putStrLn "INITIAL"
  pprint (disp init)
  putStrLn ""
  forM_ (zip [0..] happenings) $ \(n,(e,st)) → do
    putStrLn ("Move #" <> show (n∷Int))
    putStrLn (show e)
    putStrLn "->"
    pprint (disp st)
    putStrLn ""

traceTake ∷ Int → GameTrace e st → GameTrace e st
traceTake n (GameTrace st hist) = GameTrace st (take n hist)

playGame ∷ ∀m e st. Monad m => Actor m e st → GameTree e st → m (GameTrace e st)
playGame actor entireGameTree@(GameTree initialState _) = do
    thingsThatHappened ∷ [(e,st)] ← loop entireGameTree
    return $ GameTrace initialState thingsThatHappened
  where
    loop ∷ GameTree e st → m [(e,st)]
    loop tr = do
      mBranchChoosen ← actor tr
      mBranchChoosen & \case
        Nothing      →
          return []

        Just (Node happening furtherPossibilities) → do
          moreHappenings ← loop (GameTree (snd happening) furtherPossibilities)
          return (happening : moreHappenings)

dumbGame ∷ Monad m => GameTree e st -> m (GameTrace e st)
dumbGame = playGame dumbActor

randomGame ∷ GameTree e st -> IO (GameTrace e st)
randomGame = playGame randomActor

humanGame ∷ (Show e, Show st) => GameTree e st -> IO (GameTrace e st)
humanGame = playGame humanActor
