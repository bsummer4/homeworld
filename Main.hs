module Main where

import           Core
import           Game.Tree
import qualified Homeworlds.Game  as HW
import qualified Homeworlds.Move  as HW
import           Homeworlds.Types as HW
import qualified TTT.Main         as TTT

import           Control.Monad.Random
import           Control.Monad.State.Lazy
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           System.Random.Shuffle      (shuffleM)


-- Tie it all together. --------------------------------------------------------------------------------------

initialStates ∷ Int → IO [GameSt]
initialStates n = flip evalRandT (mkStdGen 0) $ shuffleM $ take n $ execGame openings
  where execGame = flip execStateT HW.emptyState
        openings = HW.joins >> HW.joins

main ∷ IO ()
main = do
  TTT.main

  LBS8.putStrLn $ A.encode exampleGame
  initial ← head <$> initialStates 10
  putStrLn "INITIAL STATE"
  putStrLn $ cs $ ppShow initial

  putStrLn "SOME GAME"
  mapM_ print $ take 2 $ gameStates $ gameTree initial HW.turns

  printTrace (view systems) $ dumbTrace $ gameTree initial HW.turns


-- Example Games ---------------------------------------------------------------------------------------------

exampleGame ∷ [Event]
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
