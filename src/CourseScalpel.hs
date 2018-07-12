module CourseScalpel
  ( ScrapeProgramRes (..)
  , ScrapeCourseRes (..)
  , CourseScalpelRunner
  , Term (..)
  , scrapeProgram
  , scrapeCourse
  , runCourseScalpel
  , mkConfig
  ) where

import           Control.Monad.IO.Class    (MonadIO)
import           Data.Semigroup            ((<>))
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc (Pretty, pretty)

import           CourseScalpel.App         (App, Config (..), runApp)
import           CourseScalpel.Course      (Course)
import CourseScalpel.Program (Program (..))
import qualified CourseScalpel.Program as Program
import qualified CourseScalpel.CoursePage  as CoursePage
import           CourseScalpel.CoursePage (MonadCoursePage (..), Term (..))
import           CourseScalpel.Error       (AppError, HasError)
import qualified CourseScalpel.ProgramPage as ProgramPage
import           CourseScalpel.ProgramPage (MonadProgramPage (..))
import           CourseScalpel.Web         (Url (..))

data ScrapeProgramRes
  = ScrapeProgramRes          [Course]
  | ScrapeProgramNetworkError AppError

instance Pretty ScrapeProgramRes where
  pretty (ScrapeProgramNetworkError err)
    =  "Network error: "
    <> pretty err

  pretty (ScrapeProgramRes courses)
    = "Program scraped! "
    <> (pretty $ length courses)
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
            <> (Program.slugToText . programSlug $ program)
        
  programPage <- scrapeProgramPage url
  coursePages <- traverse scrapeCoursePage (ProgramPage.courseUrls programPage)
  let courses = CoursePage.toCourse <$> coursePages
  pure $ ScrapeProgramRes courses
--  liftIO $ putStrLn . show $ pretty programPage
--  liftIO $ putStrLn . show $ pretty coursePage
--  

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

type CourseCode = Text

scrapeCourse
  :: forall m
  . (MonadCoursePage m, MonadIO m, HasError m)
  => CourseCode
  -> m ScrapeCourseRes
scrapeCourse code = do
  coursePage <- scrapeCoursePage . Url $ "https://liu.se/studieinfo/kurs/" <> code
  pure . ScrapeCourseSuccess $ CoursePage.toCourse coursePage
