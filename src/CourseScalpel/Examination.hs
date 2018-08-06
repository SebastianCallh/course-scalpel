{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module CourseScalpel.Examination
  ( Examination (..)
  , Credits (..)
  , Grading (..)
  , Type (..)
  ) where

import           Data.Aeson            (FromJSON, ToJSON)
import           Data.Text             (Text)
import           GHC.Generics          (Generic)

import           CourseScalpel.Credits (Credits (..))

data Examination = Examination
  { code        :: !Text
  , typ         :: !Type
  , description :: !Text
  , grading     :: !Grading
  , credits     :: !Credits
  } deriving (Show, Read, Eq, Ord,
              Generic, FromJSON, ToJSON)
data Type
  = TEN
  | LAB
  | UPG
  | AUSK
  | OPPO
  | PROJ
  | KTR
  | MUN
  | ANN
  | MOM
  | BAS
  | DAT
  | HEM
  deriving (Show, Read, Eq, Ord,
            Generic, FromJSON, ToJSON)

data Grading
  = Binary      -- U / G
  | Scale       -- U / 3 / 4 / 5
  | Presence    -- Mandatory presence
  | Unspecified -- There is a grading named "D". I do not know what it means.
  deriving (Show, Read, Eq, Ord,
            Generic, FromJSON, ToJSON)
