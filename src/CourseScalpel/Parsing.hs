{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CourseScalpel.Parsing
  ( Parseable (..)
  , Parser
  , parseError
  , takeMay
  ) where

import           CourseScalpel.Error (AppError (..), HasError, appError)
import           Data.Text           (Text)
import           Text.Megaparsec     (ErrorFancy, Parsec)

takeMay :: Int -> [a] -> Maybe [a]
takeMay n as
  | n > 0 && null as = Nothing
  | n == 0           = pure []
  | otherwise        =
    (head as :) <$> takeMay (pred n) (tail as)

--- Parsing ---

type Parser = Parsec (ErrorFancy Text) Text

class Parseable a where
  parse    :: HasError m => Text -> m a

instance Parseable a => Parseable (Maybe a) where
  parse = fmap pure . parse

parseError :: HasError m => Text -> Text -> m a
parseError txt msg = appError $ ParseError txt msg
