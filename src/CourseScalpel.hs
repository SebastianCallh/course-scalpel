module CourseScalpel
  ( ScrapeProgramRes (..)
  , ScrapeCourseRes (..)
  , CourseScalpelRunner
  , Program (..)
  , Course (..)
  , scrapeProgram
  , scrapeCourse
  , runCourseScalpel
  , mkConfig
  ) where

import           Control.Parallel.Strategies
import           Control.Monad.IO.Class    (MonadIO)
import           Data.Semigroup            ((<>))
import           Data.Text                 (Text)
import           Data.Aeson                (ToJSON (..))
import           Data.Text.Prettyprint.Doc (Pretty, pretty)
  
import           CourseScalpel.App         (App, Config (..), runApp)
import           CourseScalpel.Course      (Course (..))
import           CourseScalpel.Program     (Program (..))
import qualified CourseScalpel.Program as  Program
import qualified CourseScalpel.CoursePage  as CoursePage
import           CourseScalpel.CoursePage  (MonadCoursePage (..))
import           CourseScalpel.Error       (AppError, HasError)
import qualified CourseScalpel.ProgramPage as ProgramPage
import           CourseScalpel.ProgramPage (MonadProgramPage (..))
import           CourseScalpel.Web         (Url (..))

data ScrapeProgramRes
  = ScrapeProgramSuccess      [Course]
  | ScrapeProgramNetworkError AppError

instance ToJSON ScrapeProgramRes where
  toJSON (ScrapeProgramSuccess courses)  = toJSON courses
  toJSON (ScrapeProgramNetworkError err) = toJSON err
  
instance Pretty ScrapeProgramRes where
  pretty (ScrapeProgramNetworkError err)
    =  "Network error: "
    <> pretty err

  pretty (ScrapeProgramSuccess courses)
    = "Program scraped! "
    <> pretty (length courses)
    <> " courses were scraped successfully."

type CourseScalpelRunner a = App a -> IO (Either AppError a)

runCourseScalpel :: Config -> CourseScalpelRunner a
runCourseScalpel = runApp

mkConfig :: FilePath -> Config
mkConfig = Config

scrapeProgram
  :: forall m
  . (MonadCoursePage m, MonadProgramPage m,
     MonadIO m, HasError m)
  => Program
  -> m ScrapeProgramRes
scrapeProgram program = do
  let url = Url $ "https://liu.se/studieinfo/program/"
        <> Program.slugToText (programSlug program)
        
  programPage <- scrapeProgramPage url
  let courseUrls    = ProgramPage.courseUrls programPage
  let eCoursePages  = parMap rpar scrapeCoursePage courseUrls
  courses <- traverse (fmap CoursePage.toCourse) eCoursePages
  pure $ ScrapeProgramSuccess courses

data ScrapeCourseRes
  = ScrapeCourseSuccess      Course
  | ScrapeCourseParseError   AppError
  | ScrapeCourseNetworkError AppError

instance ToJSON ScrapeCourseRes where
  toJSON (ScrapeCourseSuccess course)   = toJSON course
  toJSON (ScrapeCourseParseError   err) = toJSON err
  toJSON (ScrapeCourseNetworkError err) = toJSON err
    
instance Pretty ScrapeCourseRes where
  pretty (ScrapeCourseSuccess course)
    =  "Course scraped: "
    <> pretty course
  pretty (ScrapeCourseParseError err)
    =  "Parsing error: "
    <> pretty err
  pretty (ScrapeCourseNetworkError err)
    =  "Network error: "
    <> pretty err

type CourseCode = Text

scrapeCourse
  :: forall m
  . (MonadCoursePage m, MonadIO m, HasError m)
  => CourseCode
  -> m ScrapeCourseRes
scrapeCourse code = do
  coursePage <- scrapeCoursePage . Url $ "https://liu.se/studieinfo/kurs/" <> code
  pure . ScrapeCourseSuccess $ CoursePage.toCourse coursePage
