{-# LANGUAGE FlexibleContexts #-}

module CourseScalpel.ProgramPage
  ( ProgramPage (..)
  , MonadProgramPage (..)
  , scraper
  , courseUrls
  , scrape
  ) where

import           Control.Applicative       (liftA2)
import           Control.Monad.IO.Class    (MonadIO)
import           Data.Aeson                (ToJSON)
import           Data.Semigroup            ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc (Pretty, pretty)
import           GHC.Generics              (Generic)
import           Text.HTML.Scalpel         hiding (scrape)
import           Text.Megaparsec           (some, (<|>))
import qualified Text.Megaparsec           as MP
import           Text.Megaparsec.Char

import qualified CourseScalpel.Course      as Course
import           CourseScalpel.Error       (parseError)
import           CourseScalpel.Parser      (Parser)
import qualified CourseScalpel.Parser      as Parser
import           CourseScalpel.Web         (Url (..))
import qualified CourseScalpel.Web         as Web

class Monad m => MonadProgramPage m where
  scrapeProgramPage :: Url -> m ScrapeResult

type ScrapeResult = Web.ScrapeResult ProgramPage

-- | A program page like the following
--   https://liu.se/studieinfo/program/6ctbi
data ProgramPage = ProgramPage
  { name     :: !Text
  , sections :: ![SpecializationSection]
  } deriving (Show, Eq, Generic)

instance ToJSON ProgramPage

instance Pretty ProgramPage where
  pretty (ProgramPage name specs)
    =  pretty name
    <> pretty specs

scrape :: MonadIO m => Url -> m ScrapeResult
scrape = Web.scrapeUrl scraper

scraper :: Scraper Text (Parser.Result ProgramPage)
scraper =
  chroot ("div" @: [hasClass "main-container"]) $ do
    header <- headerScraper
    plan   <- planScraper
    pure $ ProgramPage
      <$> (getHeader <$> header)
      <*> (planSpecs <$> plan)

newtype Header = Header { getHeader :: Text }

headerScraper :: Scraper Text (Parser.Result Header)
headerScraper = fmap parseHeader $ chroot "header" $ text "h1"
  where
    parseHeader :: Text -> Parser.Result Header
    parseHeader x =
      either (const $ parseError x "Header") pure $
      MP.parse parser "" $ T.strip x

    parser :: Parser Header
    parser = do
      name <- some (spaceChar <|> letterChar)
      _    <- char ','
      pure $ Header $ T.pack name

data Plan = Plan
  { planSpecs  :: [SpecializationSection]
  } deriving (Show, Eq)

data SpecializationSection = SpecializationSection
  { _sectionSpecialization :: !Course.Specialization
  , sectionUrls            :: ![Url]
  } deriving (Show, Eq, Generic)

instance ToJSON SpecializationSection

instance Pretty SpecializationSection where
  pretty (SpecializationSection spec urls)
    =  pretty spec
    <> pretty urls

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
courseUrls = foldMap sectionUrls . sections

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
