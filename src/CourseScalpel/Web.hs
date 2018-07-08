{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module CourseScalpel.Web
  ( Url (..)
  , scrapeError
  ) where

import           Data.Text             (Text)

import           CourseScalpel.Error   (AppError (..), HasError, appError)
import           CourseScalpel.Web.Url (Url (..))

scrapeError :: HasError m => Url -> Text -> m a
scrapeError url msg = appError $ ScrapeError url msg


