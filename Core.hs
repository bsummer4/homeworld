module Core
  ( module X
  , deriveJSON
  , enumeration
  , safeHead
  , ordNub
  , allEqual
  , pprint
  , unlessM
  ) where

import BasicPrelude            as X hiding (concat, product, sum, uncons, (<.>))
import Control.Lens            as X
import Control.Monad.Morph     as X (hoist)
import Data.Aeson              as X (FromJSON (..), ToJSON (..))
import Data.Char               as X (toLower)
import Data.Foldable           as X (concat, fold, product, sum, toList)
import Data.Monoid             as X
import Data.String.Conversions as X (convertString, cs)
import Debug.Trace             as X
import Text.Show.Pretty        as X (ppShow)

import qualified Data.Aeson.TH       as A
import qualified Data.Set            as Set
import qualified Language.Haskell.TH as TH


jsonOptions ∷ A.Options
jsonOptions = A.defaultOptions
  { A.fieldLabelModifier     = drop 1
  , A.constructorTagModifier = map toLower
  , A.sumEncoding            = A.ObjectWithSingleField
  }

deriveJSON ∷ TH.Name → TH.Q [TH.Dec]
deriveJSON = A.deriveJSON jsonOptions

enumeration ∷ (Bounded a, Enum a) ⇒ [a]
enumeration = [minBound .. maxBound]

safeHead ∷ [a] → Maybe a
safeHead = \case { [] → Nothing; x:_ → Just x }

ordNub ∷ Ord a ⇒ [a] → [a]
ordNub = Set.toList . Set.fromList

allEqual ∷ Eq a ⇒ [a] → Bool
allEqual []       = True
allEqual [_]      = True
allEqual (x:y:zs) = (x==y) && allEqual (y:zs)

pprint ∷ Show a ⇒ a → IO ()
pprint = putStrLn . cs . ppShow

unlessM ∷ Monad m ⇒ (m Bool) → m () → m ()
unlessM check action = do
  condition ← check
  unless condition $ do
    action
