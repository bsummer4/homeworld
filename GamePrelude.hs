{-# LANGUAGE TemplateHaskell #-}

module GamePrelude
  ( module Prelude
  , module Data.Map
  , module Data.Monoid
  , module Data.IntMap
  , module Text.Show.Pretty
  , module Control.Monad.Morph
  , Set
  , deriveJSON
  , enumeration
  , safeHead
  , ordNub
  , allEqual
  ) where

import Prelude

import qualified Data.Aeson.TH    as A
import           Data.Char        (toLower)
import           Data.IntMap      (IntMap)
import           Data.Map         (Map)
import           Data.Monoid
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Text.Show.Pretty (ppShow)
import           Control.Monad.Morph        (hoist)

jsonOptions = A.defaultOptions
  { A.fieldLabelModifier     = drop 1
  , A.constructorTagModifier = map toLower
  , A.sumEncoding            = A.ObjectWithSingleField
  }

deriveJSON = A.deriveJSON jsonOptions

enumeration ∷ (Bounded a, Enum a) ⇒ [a]
enumeration = [minBound .. maxBound]

safeHead ∷ [a] → Maybe a
safeHead = \case { [] → Nothing; x:_ → Just x }

ordNub ∷ Ord a ⇒ [a] → [a]
ordNub = Set.toList . Set.fromList

allEqual [] = True
allEqual [x] = True
allEqual (x:y:zs) = (x==y) && allEqual (y:zs)
