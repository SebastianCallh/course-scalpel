{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module CourseScalpel.PageCourse where
{-  ( PageCourse (..)
  , scraper
  ) where

import           Data.Text           (Text)
import           Text.HTML.Scalpel   hiding (scrape)

import           CourseScalpel.Types

data PageCourse = PageCourse
  { pageCourseAreas         :: !Areas
  , pageCourseInstitution   :: !Institution
  , pageCoursePrograms      :: !Programs
  , pageCourseFields        :: !Fields
  , pageCoursePrerequisites :: !(Maybe Prerequisites)
  , pageCourseGrading       :: !Grading
  , pageCourseExaminator    :: !(Maybe Examinator)
  , pageCourseExaminations  :: !Examinations
  , pageCourseContent       :: !CourseContent
  , pageCourseSubject       :: !Subjects
  , pageCourseUrls          :: !Urls
  , pageCourseTime          :: !CourseTime
  } deriving (Show)

scraper :: (HasError m) => Scraper Text (m PageCourse)
scraper = undefined

{-do
  sections <- chroot ("section" @: [hasClass "studyguide-block"]) $
    chroots "div" $ do
      title   <- innerHTML "h3"
      content <- sanitize . snd . T.breakOnEnd "</h3>" <$> innerHTML "div"
      return (title, content)

---  return $ bimap getError id $
  return $ fromPageSections $ M.fromList sections

-- | Strips leading and trailing whitespace and removes
--   junk characters, typically from text inside tags.
sanitize :: Text -> Text
sanitize = T.strip . T.filter (not . isTrash)
  where isTrash = (`elem` ['\t', '\n', '\r'])

fromPageSections :: HasError m => Map Text Text -> m PageCourse
fromPageSections sections =
  PageCourse
  <$> parseField Nothing              "Huvudomr\229de"
  <*> parseField Nothing              "Institution"
  <*> parseField Nothing              "Kursen ges f\246r"
  <*> parseField Nothing              "Utbildningsomr\229de"
  <*> parseField (Just Nothing)       "F\246rkunskapskrav"
  <*> parseField Nothing              "Betygsskala"
  <*> parseField (Just Nothing)       "Examinator"
  <*> parseField Nothing              "Examination"
  <*> parseField Nothing              "Kursinneh\229ll"
  <*> parseField (Just $ Subjects []) "\196mnesomr\229de"
  <*> parseField (Just $ Urls     []) "Kurshemsida och andra l\228nkar"
  <*> parseField Nothing              "Undervisningstid"
  where
    parseField :: (HasError m, Parseable a) =>  Maybe a -> Text -> m a
    parseField mDefault key =
      case M.lookup key sections of
        Nothing      -> maybe (makeError key) pure mDefault
        Just section -> parse section

    makeError secKey = appErr . ParseError $
      "Could not find section " <> secKey
-}
-}
