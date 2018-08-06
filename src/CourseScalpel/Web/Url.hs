{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module CourseScalpel.Web.Url where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Data                 (Typeable)
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc
import           GHC.Generics              (Generic)

newtype Url = Url { getUrl :: Text }
  deriving (Show, Read, Eq, Ord, Typeable, Generic, FromJSON, ToJSON)

instance Pretty Url where
  pretty (Url url) = pretty url
