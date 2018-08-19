{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CourseScalpel.Web
  ( Url (..)
  , networkError
  ) where

import           CourseScalpel.Error   (networkError)
import           CourseScalpel.Web.Url (Url (..))
