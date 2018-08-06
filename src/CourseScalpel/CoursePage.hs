{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CourseScalpel.CoursePage
  ( CoursePage (..)
  , MonadCoursePage (..)
  , module SubMods
  , scrape
  , scraper
  , toCourse
  ) where


import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Text.HTML.Scalpel         hiding (scrape)

import           CourseScalpel.Course      (Course)
import qualified CourseScalpel.Course      as Course
import qualified CourseScalpel.Parser as Parser
import           CourseScalpel.Web         (Url (..))

import           CourseScalpel.CoursePage.Header   as SubMods (Header (..))
import qualified CourseScalpel.CoursePage.Header   as Header
import           CourseScalpel.CoursePage.Programs as SubMods (Programs (..))
import qualified CourseScalpel.CoursePage.Programs as Programs
import           CourseScalpel.CoursePage.Plan     as SubMods (Plan (..))
import qualified CourseScalpel.CoursePage.Plan     as Plan

data CoursePage = CoursePage
  { header   :: !Header
  , programs :: !Programs
  , plan     :: !Plan
  } deriving (Show, Eq)

class Monad m => MonadCoursePage m where
  scrapeCoursePage :: Url -> m CoursePage
 
scrape :: Url -> IO (Parser.Result CoursePage)
scrape url =
  scrapeURL (T.unpack $ getUrl url) scraper >>= \case
    Nothing          -> pure . Left $ Parser.NetworkError url
    Just eCoursePage -> pure eCoursePage

scraper :: Scraper Text (Parser.Result CoursePage)
scraper = do
  header   <- Header.scraper
  programs <- Programs.scraper
  plan     <- Plan.scraper
  pure $ CoursePage
    <$> header
    <*> programs
    <*> plan

toCourse :: CoursePage ->  Course
toCourse CoursePage{..} = Course.Course
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
