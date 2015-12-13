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

data GameTree e st     = GameTree st (Forest (e,st)) deriving (Eq,Show,Functor,Foldable,Traversable)
data GameTrace e st    = Trace st [(e,st)]           deriving (Eq,Ord,Show,Functor,Foldable,Traversable)
type Actor m e st      = GameTree e st -> m (Maybe (e, GameTree e st))
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
dumbActor (GameTree _ elems) = return $ f <$> safeHead elems
  where f (Node (e, st) forest) = (e, GameTree st forest)

randomActor ∷ ActorIO e s
randomActor (GameTree _ elems) = fmap f . safeHead <$> shuffleM elems
  where
    f (Node (e, st) forest) = (e, GameTree st forest)

interleaveActs ∷ [Actor m e st] -> IO (Actor m e st)
interleaveActs _actors = undefined

humanActor ∷ (Show e, Show st) => ActorIO e st
humanActor = undefined

naiveAI ∷ Scoring st -> Actor m e st
naiveAI = undefined

lookaheadAI ∷ Int -> Scoring st -> Actor m e st
lookaheadAI = undefined


-- Operations on Traces --------------------------------------------------------------------------------------

traceEvents ∷ GameTrace e st → [e]
traceEvents (Trace _ hist) = fst <$> hist

printTraceHistory ∷ (Show e, Show st) ⇒ GameTrace e st → IO ()
printTraceHistory tr@(Trace init _) = do
  print init
  forM_ (traceEvents tr) $ \e → do
    print e

printTrace ∷ (Show e, Show a) ⇒ (st → a) → GameTrace e st → IO ()
printTrace disp (Trace init happenings) = do
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
traceTake n (Trace st hist) = Trace st (take n hist)

playGame ∷ Monad m => Actor m e st → GameTree e st → m (GameTrace e st)
playGame actor init@(GameTree initialState _) = Trace initialState <$> loop init
  where
    loop tr = do
      actor tr >>= \case
        Nothing → return []
        Just (e, subtree@(GameTree st' _)) → ((e, st'):) <$> loop subtree

dumbGame ∷ Monad m => GameTree e st -> m (GameTrace e st)
dumbGame = playGame dumbActor

randomGame ∷ GameTree e st -> IO (GameTrace e st)
randomGame = playGame randomActor

dumbTrace ∷ ∀e st. GameTree e st → GameTrace e st
dumbTrace (GameTree init children) = traceTake 100 $ Trace init $ loop children
  where loop ∷ [Tree (e,st)] → [(e,st)]
        loop = \case []                    → []
                     Node (e,st) below : _ → (e,st) : loop below
