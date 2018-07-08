module CourseScalpel
  ( ScrapeProgramRes (..)
  , ScrapeCourseRes (..)
  , CourseScalpelRunner
  , scrapeProgram
  , scrapeCourse
  , runCourseScalpel
  , mkConfig
  ) where

import           Control.Monad.IO.Class    (MonadIO)
import           Data.Semigroup            ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc (Pretty, pretty)

import           CourseScalpel.App         (App, runApp, Config (..))
import           CourseScalpel.Course      (Course)
import qualified CourseScalpel.CoursePage  as CoursePage
import           CourseScalpel.Error       (AppError, HasError)
import           CourseScalpel.Program     (Program (..))
import qualified CourseScalpel.ProgramPage as ProgramPage
import           CourseScalpel.Web         (Url (..))

data ScrapeProgramRes
  = ScrapeProgramRes         [AppError] [Course]
  | ScrapeProgramNetworkError AppError

instance Pretty ScrapeProgramRes where
  pretty (ScrapeProgramNetworkError err)
    =  "Network error: "
    <> pretty err
    
  pretty (ScrapeProgramRes errs listCourses)
    = "Program scraped! "
    <> (pretty $ length listCourses)
    <> " courses were scraped successfully and "
    <> (pretty $ length errs)
    <> " failed."

type CourseScalpelRunner a = App a -> IO (Either AppError a)

runCourseScalpel :: Config -> CourseScalpelRunner a
runCourseScalpel = runApp

mkConfig :: FilePath -> Config
mkConfig = Config

scrapeProgram :: forall m. (MonadIO m, HasError m) => Program -> m ScrapeProgramRes
scrapeProgram program = do
  programPage <- ProgramPage.scrape $ programToUrl program
  let coursePages = CoursePage.scrape <$> ProgramPage.courseUrls programPage
  let courseEs    = fmap CoursePage.toCourse <$> coursePages :: [m Course]
  ScrapeProgramRes [] <$> sequence courseEs
  
  where
    programToUrl :: Program -> Url
    programToUrl =
      Url . (<>) "https://liu.se/studieinfo/program/" .
      T.tail . T.pack . show . programSlug

data ScrapeCourseRes
  = ScrapeCourseSuccess      Course
  | ScrapeCourseParseError   AppError
  | ScrapeCourseNetworkError AppError

instance Pretty ScrapeCourseRes where
  pretty (ScrapeCourseSuccess      course)
    =  "Course scraped: "
    <> pretty course
    
  pretty (ScrapeCourseParseError   err)
    =  "Parsing error: "
    <> pretty err
    
  pretty (ScrapeCourseNetworkError err)
    =  "Network error: "
    <> pretty err

scrapeCourse :: forall m. (MonadIO m, HasError m) => Text -> m ScrapeCourseRes
scrapeCourse code = do
  coursePage <- CoursePage.scrape $ courseCodeToUrl code
  pure . ScrapeCourseSuccess $ CoursePage.toCourse coursePage
  where
    courseCodeToUrl :: Text -> Url
    courseCodeToUrl = Url . (<>) "https://liu.se/studieinfo/kurs/"    
