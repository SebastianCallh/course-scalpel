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

import           CourseScalpel.App         (App, runApp, Config (..))
import           CourseScalpel.Course      (Course)
import qualified CourseScalpel.CoursePage  as CoursePage
import           CourseScalpel.Error       (AppError, HasError)
import           CourseScalpel.Program     (Program (..))
import qualified CourseScalpel.ProgramPage as ProgramPage
import           CourseScalpel.Web         (Url (..))

data ScrapeProgramRes
  = ScrapeProgramRes         [AppError] [Course]
  | ScrapeProgramNetworkFail AppError

instance Show ScrapeProgramRes where
  show (ScrapeProgramNetworkFail err) = "Network failure: " <> show err
  show (ScrapeProgramRes errs listCourses) = mconcat
    [ "Program scraped! "
    , show $ length listCourses
    , " courses were scraped successfully and "
    , show $ length errs
    , " failed."
    ]

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

instance Show ScrapeCourseRes where
  show (ScrapeCourseSuccess      _ ) = "Course scraped!"
  show (ScrapeCourseParseError   err) = "Parsing failure: " <> show err
  show (ScrapeCourseNetworkError err) = "Network failure: " <> show err

scrapeCourse :: forall m. (MonadIO m, HasError m) => Text -> m ScrapeCourseRes
scrapeCourse code = do
  coursePage <- CoursePage.scrape $ courseCodeToUrl code
  pure . ScrapeCourseSuccess $ CoursePage.toCourse coursePage

{-do
  eCoursePage <- CoursePage.scrape $ courseCodeToUrl code
  pure $ case eCoursePage of
    Left  error      -> ScrapeCourseNetworkError error
    Right coursePage -> ScrapeCourseSuccess $ CoursePage.toCourse coursePage
  -}
  where
    courseCodeToUrl :: Text -> Url
    courseCodeToUrl = Url . (<>) "https://liu.se/studieinfo/kurs/"
      
{-do
  let url = courseCodeToUrl courseCode
  mePageCourses <- scrapeURL (T.unpack $ getUrl url)
    undefined -- PageCourse.scrapera

  return $ case mePageCourses of
    Nothing          ->
      ScrapeCourseNetworkFail $ networkError url
    Just ePageCourse ->
      either ScrapeCourseParseFail ScrapeCourseSuccess ePageCourse
-}

--networkError :: Url -> Text
--networkError url = "Scalpel returned Nothing for url " <> getUrl url
