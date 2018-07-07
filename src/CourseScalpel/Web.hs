{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module CourseScalpel.Web
  ( Scrapeable (..)
  , Url (..)
  , scrapeError
  ) where

import           Data.Text             (Text)

import           CourseScalpel.Error   (AppError (..), HasError, appError)
import           CourseScalpel.Web.Url (Url (..))

--- Scrapable ---

class Scrapeable a where
  scrapeUrl  :: (HasError m) => Url      -> m a
  scrapeFile :: (HasError m) => FilePath -> m a

scrapeError :: (HasError m) => Url -> Text -> m a
scrapeError url msg = appError $ ScrapeError url msg
