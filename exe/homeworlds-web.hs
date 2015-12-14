{-# LANGUAGE CPP #-}

module Main(main) where

import           Core
import           GameTree
import qualified Homeworlds.Game  as HW
import qualified Homeworlds.Move  as HW
import           Homeworlds.Types as HW

#ifndef WEB
main ∷ IO ()
main = putStrLn "This is a stub. I can't yet get reflex-dom to compile with GHCJS."
#else

import Reflex
import Reflex.Dom

main ∷ IO ()
main = do
  mainWidget $
    el "h1" (text "Hello Reflex!")

#endif
