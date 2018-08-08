{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CourseScalpel.Time
  ( Time (..)
  , Hours (..)
  ) where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Word  (Word)

newtype Time = Time { getTime :: Word }
  deriving (Show, Read, Eq, Ord, FromJSON, ToJSON, Num)

newtype Hours = Hours { getHours :: Word }
  deriving (Show, Read, Eq, Ord, FromJSON, ToJSON, Num)
