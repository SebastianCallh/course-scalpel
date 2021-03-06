module CourseScalpel
  ( ScrapeResult (..)
  , module X
  , scrapeProgramPage
  , scrapeCoursePage
  , runCourseScalpel
  , mkConfig
  ) where

import           Data.Semigroup                    ((<>))
import           Data.Text                         (Text)

import           CourseScalpel.App                 (App, Config (..), runApp)
import           CourseScalpel.CoursePage          as X (CoursePage,
                                                         MonadCoursePage)
import qualified CourseScalpel.CoursePage          as CoursePage
import           CourseScalpel.CoursePage.Occasion as X (Block (..),
                                                         Importance (..),
                                                         Occasion (..),
                                                         Period (..),
                                                         Semester (..))
import           CourseScalpel.Program             (Program)
import           CourseScalpel.Program             as X (engD, engDPU, engED,
                                                         engEMM, engI, engIInt,
                                                         engIT, engKB, engKTS,
                                                         engM, engMT, engMed,
                                                         engTB, engU, engY,
                                                         engYInt,
                                                         supportedPrograms)
import qualified CourseScalpel.Program             as Program
import           CourseScalpel.ProgramPage         as X (MonadProgramPage,
                                                         ProgramPage)
import qualified CourseScalpel.ProgramPage         as ProgramPage
import           CourseScalpel.Web                 (ScrapeResult (..), Url (..))

runCourseScalpel :: Config -> App a -> IO a
runCourseScalpel = runApp

mkConfig :: FilePath -> Config
mkConfig = Config

scrapeProgramPage :: MonadProgramPage m => Program -> m (ScrapeResult ProgramPage)
scrapeProgramPage program = ProgramPage.scrapeProgramPage url
  where
    url = Url $ "https://liu.se/studieinfo/program/"
          <> Program.slugToText (Program.slug program)

scrapeCoursePage :: MonadCoursePage m => Text -> m CoursePage.ScrapeResult
scrapeCoursePage code = CoursePage.scrapeCoursePage url
  where
    url = Url $"https://liu.se/studieinfo/kurs/" <> code
