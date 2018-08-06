{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CourseScalpel.ProgramPage
  ( ProgramPage (..)
  , MonadProgramPage (..)
  , SpecializationSection (..)
  , Content (..)
  , contentScraper
  , courseUrls
  , scrape
  ) where


import           Control.Applicative       (liftA2)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc
import           Data.Validation           (Validation (..), toEither)
import           Text.HTML.Scalpel         hiding (scrape)
import           Text.Megaparsec           (some, (<|>))
import qualified Text.Megaparsec           as MP
import           Text.Megaparsec.Char

import qualified CourseScalpel.Course      as Course
import           CourseScalpel.Error       (HasError)
import           CourseScalpel.Parser     (Parser)
import           CourseScalpel.Web         (Url (..), scrapeError)

-- | A program page like the following
--   https://liu.se/studieinfo/program/6ctbi
data ProgramPage = ProgramPage
  { programPageName            :: !Text
  , programPageSpecializations :: ![SpecializationSection]
  } deriving (Show, Eq)

class Monad m => MonadProgramPage m where
  scrapeProgramPage :: Url -> m ProgramPage
  
data Error
  = ParseError   Text Message
  | NetworkError Url  Message
  deriving (Show, Eq)

instance Pretty Error where
  pretty (ParseError txt msg)
    =  pretty ("Parse error: " :: Text)
    <> pretty txt
    <> pretty msg

  pretty (NetworkError url msg)
    =  pretty ("Network error: " :: Text)
    <> pretty url
    <> pretty msg

type Result a = Validation [Error] a

type Message = Text

parseError :: Text -> Message -> Validation [Error] a
parseError txt msg = Failure [ParseError txt msg]

networkError :: Url -> Message -> Validation [Error] a
networkError url msg = Failure [NetworkError url msg]

scrape :: (HasError m, MonadIO m) => Url -> m ProgramPage
scrape url = do
  content <- scrapeContent url
  let eProgramPage = ProgramPage
        <$> (contentName  <$> content)
        <*> (contentSpecs <$> content)

  case toEither eProgramPage of
    Left  errors -> scrapeError url $ concatErrorMessages errors
    Right page   -> pure page

  where
    concatErrorMessages :: [Error] -> Text
    concatErrorMessages errors =
      T.concat . flip map errors $ \case
      (ParseError   _ msg) -> msg
      (NetworkError _ msg) -> msg

data Content = Content
  { contentName  :: !Text
  , contentSpecs :: ![SpecializationSection]
  } deriving (Show, Eq)

scrapeContent :: MonadIO m => Url -> m (Result Content)
scrapeContent url = do
  meContent <- liftIO $ scrapeURL (T.unpack $ getUrl url) contentScraper
  maybe (pure $ networkError url "Content") pure meContent

contentScraper :: Scraper Text (Result Content)
contentScraper =
  chroot ("div" @: [hasClass "main-container"]) $ do
    header <- headerScraper
    plan   <- planScraper
    pure $ Content
      <$> (getHeader <$> header)
      <*> (planSpecs <$> plan)

newtype Header = Header { getHeader :: Text }

headerScraper :: Scraper Text (Result Header)
headerScraper = fmap parseHeader $ chroot "header" $ text "h1"

parseHeader :: Text -> Result Header
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

planScraper :: Scraper Text (Result Plan)
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

parseSpecialization :: Text -> Result Course.Specialization
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
