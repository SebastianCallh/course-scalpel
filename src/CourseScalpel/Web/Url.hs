{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module CourseScalpel.Web.Url where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Data                 (Typeable)
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc
import           GHC.Generics              (Generic)

{-
newtype Urls = Urls { getUrls :: [Url] }
  deriving (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)

instance Parseable Urls where
  parse x = pure . Urls $ maybe [] (fmap Url) $
    scrapeStringLike x $ attrs "href" "a"
-}

newtype Url = Url { getUrl :: Text }
  deriving (Show, Read, Eq, Ord, Typeable, Generic, FromJSON, ToJSON)

instance Pretty Url where
  pretty (Url url) = pretty url
