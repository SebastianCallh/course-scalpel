{-# LANGUAGE FlexibleContexts #-}

module CourseScalpel.ProgramPage
  ( ProgramPage (..)
  , MonadProgramPage (..)
  , SpecializationSection (..)
  , Content (..)
  , contentScraper
  , courseUrls
  , scrape
  ) where

import           Control.Applicative    (liftA2)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Text.HTML.Scalpel      hiding (scrape)
import           Text.Megaparsec        (some, (<|>))
import qualified Text.Megaparsec        as MP
import           Text.Megaparsec.Char

import qualified CourseScalpel.Course   as Course
import           CourseScalpel.Error    (networkError, parseError)
import           CourseScalpel.Parser   (Parser)
import qualified CourseScalpel.Parser   as Parser
import           CourseScalpel.Web      (Url (..))

-- | A program page like the following
--   https://liu.se/studieinfo/program/6ctbi
data ProgramPage = ProgramPage
  { programPageName            :: !Text
  , programPageSpecializations :: ![SpecializationSection]
  } deriving (Show, Eq)

class Monad m => MonadProgramPage m where
  scrapeProgramPage :: Url -> m (Parser.Result ProgramPage)

scrape :: MonadIO m => Url -> m (Parser.Result ProgramPage)
scrape url = do
  content <- scrapeContent url
  pure $ ProgramPage
    <$> (contentName  <$> content)
    <*> (contentSpecs <$> content)

data Content = Content
  { contentName  :: !Text
  , contentSpecs :: ![SpecializationSection]
  } deriving (Show, Eq)

scrapeContent :: MonadIO m => Url -> m (Parser.Result Content)
scrapeContent url = do
  meContent <- liftIO $ scrapeURL (T.unpack $ getUrl url) contentScraper
  pure $ fromMaybe (networkError url) meContent

contentScraper :: Scraper Text (Parser.Result Content)
contentScraper =
  chroot ("div" @: [hasClass "main-container"]) $ do
    header <- headerScraper
    plan   <- planScraper
    pure $ Content
      <$> (getHeader <$> header)
      <*> (planSpecs <$> plan)

newtype Header = Header { getHeader :: Text }

headerScraper :: Scraper Text (Parser.Result Header)
headerScraper = fmap parseHeader $ chroot "header" $ text "h1"

parseHeader :: Text -> Parser.Result Header
parseHeader x =
  either (const $ parseError x "Header") pure $
    MP.parse parser "" $ T.strip x
  where
    parser :: Parser Header
    parser = do
      name <- some (spaceChar <|> letterChar)
      _    <- char ','
      pure $ Header $ T.pack name

data Plan = Plan
  { planSpecs  :: [SpecializationSection]
  } deriving (Show, Eq)

data SpecializationSection = SpecializationSection
  { specSecSpec :: !Course.Specialization
  , specSecUrls :: ![Url]
  } deriving (Show, Eq)

planScraper :: Scraper Text (Parser.Result Plan)
planScraper =
  chroot ("div" @: [hasClass "programplan"]) $ do
    specAttrs <- attrs "data-specialization" $
      "div" @: [hasClass "specialization"]

    urlAttrs <- chroots ("div" @: [hasClass "specialization"]) $
      attrs "href" "a"

    let specs    = parseSpecialization   <$> specAttrs
    let urls     = traverse (pure . Url) <$> urlAttrs
    let specSecs = sequenceA $ zipWith (liftA2 SpecializationSection) specs urls
    pure $ Plan <$> specSecs

courseUrls :: ProgramPage -> [Url]
courseUrls = foldMap specSecUrls . programPageSpecializations

parseSpecialization :: Text -> Parser.Result Course.Specialization
parseSpecialization "programmeringochalgoritmer"       = pure Course.SpecializationAlgorithms
parseSpecialization "kommunikation"                    = pure Course.SpecializationCommunication
parseSpecialization "datorsystem"                      = pure Course.SpecializationComputerSystems
parseSpecialization "elektronik"                       = pure Course.SpecializationElectronics
parseSpecialization "spelprogrammering"                = pure Course.SpecializationGames
parseSpecialization "industriellekonomi"               = pure Course.SpecializationIndustrialEconomics
parseSpecialization "internationalsoftwareengineering" = pure Course.SpecializationInternational
parseSpecialization "aiochmaskininlärning"             = pure Course.SpecializationMachineLearning
parseSpecialization "medicinskinformatik"              = pure Course.SpecializationMedicinalInformatics
parseSpecialization "signal-ochbildbehandling"         = pure Course.SpecializationSignalProcessing
parseSpecialization "säkrasystem"                      = pure Course.SpecializationSafeSystems
parseSpecialization "storskaligmjukvaruutveckling"     = pure Course.SpecializationSoftwareEngineering
parseSpecialization "systemteknologi"                  = pure Course.SpecializationSystemsTechnology
parseSpecialization "system-on-chip"                   = pure Course.SpecializationSystemOnChip
parseSpecialization "" = pure Course.SpecializationNone
parseSpecialization x  = parseError x "Specialization"
