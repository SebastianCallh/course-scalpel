{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CourseScalpel.CoursePage
  ( CoursePage
  , ScrapeResult
  , MonadCoursePage (..)
  , module X
  , scrape
  , scraper
  , course
  , occasions
  ) where

import           CourseScalpel.CoursePage.Header   as X (Header (..))
import           CourseScalpel.CoursePage.Occasion as X (Occasion (..))
import           CourseScalpel.CoursePage.Plan     as X (Plan (..))

import           Control.Monad.IO.Class            (MonadIO)
import           Data.Aeson                        (ToJSON)
import           Data.Semigroup                    ((<>))
import           Data.Text                         (Text)
import           Data.Text.Prettyprint.Doc         (Pretty, pretty)
import           GHC.Generics                      (Generic)
import           Text.HTML.Scalpel                 hiding (scrape)

import           CourseScalpel.Course              (Course)
import qualified CourseScalpel.Course              as Course
import qualified CourseScalpel.CoursePage.Header   as Header
import qualified CourseScalpel.CoursePage.Occasion as Occasion
import qualified CourseScalpel.CoursePage.Plan     as Plan
import qualified CourseScalpel.Parser              as Parser
import           CourseScalpel.Web                 (Url (..))
import qualified CourseScalpel.Web                 as Web

type ScrapeResult = Web.ScrapeResult CoursePage

data CoursePage = CoursePage
  { header    :: !Header
  , occasions :: ![Occasion]
  , plan      :: !Plan
  } deriving (Show, Eq, Generic)

instance ToJSON CoursePage

instance Pretty CoursePage where
  pretty CoursePage{..}
    =  pretty header
    <> pretty occasions
    <> pretty plan

class Monad m => MonadCoursePage m where
  scrapeCoursePage :: Url -> m ScrapeResult

scrape :: MonadIO m => Url -> m ScrapeResult
scrape = Web.scrapeUrl scraper

scraper :: Scraper Text (Parser.Result CoursePage)
scraper = do
  header    <- Header.scraper
  occasions <- Occasion.scraper
  plan      <- Plan.scraper
  pure $ CoursePage
    <$> header
    <*> occasions
    <*> plan

course :: CoursePage ->  Course
course CoursePage{..} = Course.Course
  { Course.code          = Header.code        header
  , Course.name          = Header.name        header
  , Course.level         = Plan.level         plan
  , Course.areas         = Plan.areas         plan
  , Course.institution   = Plan.institution   plan
  , Course.fields        = Plan.fields        plan
  , Course.prerequisites = Plan.prerequisites plan
  , Course.examinator    = Plan.examinator    plan
  , Course.examinations  = Plan.examinations  plan
  , Course.content       = Plan.content       plan
  , Course.subjects      = Plan.subjects      plan
  , Course.selfStudyTime = Plan.selfStudyTime plan
  , Course.scheduledTime = Plan.scheduledTime plan
  , Course.urls          = Plan.urls          plan
  }
