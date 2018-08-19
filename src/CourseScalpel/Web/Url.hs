module CourseScalpel.Web.Url where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc (Pretty, pretty)
import           GHC.Generics              (Generic)

newtype Url = Url { getUrl :: Text }
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON Url
instance ToJSON Url

instance Pretty Url where
  pretty (Url url) = pretty url
