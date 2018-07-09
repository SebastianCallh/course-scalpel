module CourseScalpel.Time
  ( MonadTime (..)
  , Year (..)
  ) where

class Monad m => MonadTime m where
  currentYear :: m Year

newtype Year = Year { getYear :: Word }
