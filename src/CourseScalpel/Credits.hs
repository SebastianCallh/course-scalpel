{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Gets it's own module to use
-- GeneralizedNewtypeDeriving for Num instance.
module CourseScalpel.Credits
  ( Credits (..)
  ) where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Monoid               (Monoid (..))
import           Data.Text.Prettyprint.Doc
import           GHC.Generics              (Generic)

newtype Credits = Credits Float
  deriving (Show, Read, Eq, Ord, Num,
            Generic, FromJSON, ToJSON)

instance Monoid Credits where
  mappend (Credits x) (Credits y) = Credits $ x + y
  mempty                          = Credits 0

instance Pretty Credits where
  pretty (Credits x) =
    pretty x <> " hp"

