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
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                   as Set
import           Debug.Trace
import           System.Random.Shuffle      (shuffleM)
import           Text.Show.Pretty           (ppShow)
import           Control.Monad.Random

import           THUtils
import           Types
import           Moves
import qualified Games


-- Tie it all together. --------------------------------------------------------------------------------------

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
            Nothing  → putStrLn "Invalid move! aborting."
            Just st' → loop st' es

main ∷ IO ()
main = do
  LBS8.putStrLn $ A.encode exampleGame

  let allInitialStates = flip execStateT emptyState $ do
        Games.joins
        Games.joins

  initial ← fmap head $ flip evalRandT (mkStdGen 0) $ shuffleM $ take 10 allInitialStates

  putStrLn "INITIAL STATE"
  putStrLn $ ppShow initial

  putStrLn "How deep should we go?"
  depth ← read <$> getLine

  let allFollowupStates = flip execStateT initial $ replicateM depth Games.simpleActionSeqs

  print (length allFollowupStates)

  someGames ← take 100 <$> flip evalRandT (mkStdGen 0) (shuffleM allFollowupStates)

  -- mapM_ (\x → print x >> putStrLn "") $ reverse <$> view history <$> someGames

  executeEventLog $ reverse $ view history $ head someGames


-- Example Games ---------------------------------------------------------------------------------------------

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
