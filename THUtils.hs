{-# LANGUAGE TemplateHaskell #-}

module THUtils where

import Data.Aeson.TH as A
import Data.Char (toLower)

jsonOptions = A.defaultOptions
  { A.fieldLabelModifier     = drop 1
  , A.constructorTagModifier = map toLower
  , A.sumEncoding            = A.ObjectWithSingleField
  }

deriveJSON = A.deriveJSON jsonOptions
