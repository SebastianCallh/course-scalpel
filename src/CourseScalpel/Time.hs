{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CourseScalpel.Time
  ( Time (..)
  ) where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Word  (Word)

newtype Time = Time { getTime :: Word }
  deriving (Show, Read, Eq, Ord, FromJSON, ToJSON, Num)
