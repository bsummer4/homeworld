module Main where

import           Core
import           GameTree
import qualified Homeworlds.Game  as HW
import qualified Homeworlds.Move  as HW
import           Homeworlds.Types as HW hiding (Event)

import           Control.Concurrent
import qualified Prelude
import           Reflex
import           Reflex.Dom

asyncComputeRandomGame :: MonadWidget t m ⇒ Event t () → m (Event t String)
asyncComputeRandomGame signal =
    performEventAsync $ ffor signal $ \_ cb → do
        void $ liftIO $ forkIO $ do
            cb "Generating a game!\n\nIt can take a couple of seconds if the game tree gets too large."
            result ← randomGame $ gameTree HW.emptyState HW.events
            cb (ppShow result)

main ∷ IO ()
main = do
    mainWidget $ do
        el "h1" (text "Homeworlds")

        ePlayBtn      ← button "Play a random game!"

        eComputedGame ← asyncComputeRandomGame ePlayBtn
        dynGameStr    ← holdDyn "^ Push this to generate games!" eComputedGame

        el "pre" (dynText dynGameStr)


-- Notes -----------------------------------------------------------------------------------------------------

-- performEventAsync :: ∀t m a. MonadWidget t m => Event t ((a -> IO ()) -> WidgetHost m ()) -> m (Event t a)

-- def :: Default a => a

-- data TextArea t = ...
--   instance HasValue (TextArea t)
--   instance type Value (TextArea t) = Dynamic t String

-- class HasValue a where Source
--   type Value a :: *
--   value        :: a -> Value a

-- button         :: MonadWidget t m => String           -> m (Event t ())
-- buttonWithIcon :: MonadWidget t m => String -> String -> m (Event t ())
-- textArea       :: MonadWidget t m => TextAreaConfig t -> m (TextArea t)
-- el             :: MonadWidget t m => String -> m a    -> m a
-- text           :: MonadWidget t m => String           -> m ()

-- dynText    :: MonadWidget t m           => Dynamic t String        -> m ()
-- display    :: (MonadWidget t m, Show a) => Dynamic t a             -> m ()
-- dyn        :: MonadWidget t m           => Dynamic t (m a)         -> m (Event t a)
-- constDyn   :: Reflex t => a                                        -> Dynamic t a
-- mapDyn     :: (Reflex t, MonadHold t m) => (a -> b) -> Dynamic t a -> m (Dynamic t b)
-- value      :: MonadWidget t m           => TextArea t              -> Dynamic t String

-- mainWidget :: ∀wtf. (wtf ~ Gui Spider (WithWebView SpiderHost) (HostFrame Spider))
--                                         => Widget Spider wtf () -> IO ()
