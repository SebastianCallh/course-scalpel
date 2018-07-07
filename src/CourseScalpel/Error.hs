{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module CourseScalpel.Error
  ( AppError (..)
  , HasError
  , appError
  ) where

import           Control.Monad.Except  (MonadError, throwError)
import           Data.Semigroup        (Semigroup (..), (<>))
import           Data.Text             (Text)

import           CourseScalpel.Web.Url (Url (..))

type ErrorMessage = Text

data AppError
  = ParseError  Text ErrorMessage
  | ScrapeError Url  ErrorMessage
  | MultipleErrors [AppError]
  deriving (Show, Eq)

instance Semigroup AppError where
  (<>) (MultipleErrors es) (ParseError txt msg)  = MultipleErrors $ ParseError  txt msg : es
  (<>) (ParseError txt msg)  (MultipleErrors es) = MultipleErrors $ ParseError  txt msg : es
  (<>) (MultipleErrors es) (ScrapeError url msg) = MultipleErrors $ ScrapeError url msg : es
  (<>) (ScrapeError url msg) (MultipleErrors es) = MultipleErrors $ ScrapeError url msg : es
  (<>) (MultipleErrors es) (MultipleErrors es')  = MultipleErrors $ es <> es'
  (<>) (ParseError txt msg)  (ParseError txt' msg')  =
    MultipleErrors [ParseError txt msg, ParseError txt' msg']
  (<>) (ScrapeError url msg) (ScrapeError url' msg') =
    MultipleErrors [ScrapeError url msg, ScrapeError url' msg']
  (<>) (ParseError txt msg)  (ScrapeError url msg')  =
    MultipleErrors [ParseError txt msg, ScrapeError url msg']
  (<>) (ScrapeError url msg) (ParseError txt msg')   =
    MultipleErrors [ScrapeError url msg, ParseError txt msg']

-- | May want to change error handling, so it gets its own type
type HasError = MonadError AppError

-- | May want to change error handling so wrap throwError
appError :: HasError m => AppError -> m a
appError = throwError

