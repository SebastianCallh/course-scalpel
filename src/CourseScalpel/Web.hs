{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CourseScalpel.Web
  ( Url (..)
  , ScrapeResult (..)
  , scrapeUrl
--  , networkError
  ) where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Aeson                (ToJSON)
import           Data.Semigroup            ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc (Pretty, pretty)
import           GHC.Generics              (Generic)
import           Text.HTML.Scalpel         hiding (scrape)

import           CourseScalpel.Error       (Error)
import qualified CourseScalpel.Parser      as Parser
import           CourseScalpel.Web.Url     (Url (..))

data ScrapeResult a
  = ScrapeSuccess a
  | ScrapeFail    Error
  | NetworkError  Url
  deriving Generic

instance ToJSON a => ToJSON (ScrapeResult a)

instance Pretty a => Pretty (ScrapeResult a) where
  pretty (NetworkError url)
    =  "Could not query url "
    <> pretty url

  pretty (ScrapeFail err)
    =  "Error scraping program page: "
    <> pretty err

  pretty (ScrapeSuccess program)
    = "Program scraped!\n"
    <> pretty program

scrapeUrl :: MonadIO m => Scraper Text (Parser.Result a) -> Url-> m (ScrapeResult a)
scrapeUrl scraper url = do
  mepage <- liftIO $ scrapeURL (T.unpack $ getUrl url) scraper
  pure $ case mepage of
    Nothing    -> NetworkError url
    Just epage -> case epage of
      Left err   -> ScrapeFail err
      Right page -> ScrapeSuccess page
