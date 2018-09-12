module CourseScalpel
  ( ProgramPageScrapeResult
  , CoursePageScrapeResult
  , CourseScalpelRunner
  , module X
  , scrapeProgramPage
  , scrapeCoursePage
  , runCourseScalpel
  , mkConfig
  ) where

import           Data.Text                         (Text)
import           Data.Semigroup                    ((<>))

import qualified CourseScalpel.CoursePage          as CoursePage
import           CourseScalpel.App                 (App, Config (..), runApp)
import           CourseScalpel.Program             (Program)
import qualified CourseScalpel.Program             as Program
import           CourseScalpel.Error               (Error (..), MonadError)
import qualified CourseScalpel.ProgramPage         as ProgramPage
import           CourseScalpel.Web                 (Url (..))
import           CourseScalpel.CoursePage          as X (CoursePage, MonadCoursePage)
import           CourseScalpel.ProgramPage         as X (MonadProgramPage)
import           CourseScalpel.CoursePage.Occasion as X (Occasion (..), Semester (..)
                                                        , Period (..), Block (..)
                                                        , Importance (..))
import           CourseScalpel.Program             as X (engD, engU, engI, engIInt
                                                        , engIT, engY, engYInt, engMed
                                                        , engMT, engED, engKTS, engM
                                                        , engEMM, engTB, engDPU, engKB
                                                        , supportedPrograms)

type ProgramPageScrapeResult = ProgramPage.ScrapeResult 
type CoursePageScrapeResult = CoursePage.ScrapeResult 
type CourseScalpelRunner a = App a -> IO (Either Error a)

runCourseScalpel :: Config -> CourseScalpelRunner a
runCourseScalpel = runApp

mkConfig :: FilePath -> Config
mkConfig = Config

scrapeProgramPage
  :: forall m
  . (MonadProgramPage m,
     MonadError m)
  => Program
  -> m ProgramPage.ScrapeResult
scrapeProgramPage program = ProgramPage.scrapeProgramPage url
  where
    url = Url $ "https://liu.se/studieinfo/program/"
          <> Program.slugToText (Program.slug program)

scrapeCoursePage
  :: forall m
  . (MonadCoursePage m,
     MonadError m)
  => Text
  -> m CoursePage.ScrapeResult
scrapeCoursePage code = CoursePage.scrapeCoursePage url
  where
    url = Url $"https://liu.se/studieinfo/kurs/" <> code
