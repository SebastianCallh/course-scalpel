module CourseScalpel
  ( ProgramPageScrapeResult
  , ScrapeCoursePageResult (..)
  , CourseScalpelRunner
  , module X
  , scrapeProgramPage
  , scrapeCoursePage
  , runCourseScalpel
  , mkConfig
  ) where

import           GHC.Generics              (Generic)
--import           Control.Parallel.Strategies
import           Data.Text                 (Text)
import           Data.Semigroup            ((<>))
import           Data.Aeson                (ToJSON (..))
import           Data.Text.Prettyprint.Doc (Pretty, pretty)
--import           Data.Either               (partitionEithers)

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

{-
data ScrapeProgramPageResult
  = ScrapeProgramPageSuccess [Error] [CoursePage]
  | ScrapeProgramPageError   Error

instance ToJSON ScrapeProgramPageResult where
  toJSON (ScrapeProgramPageSuccess errors pages) =
    object [ "errors" .= toJSON errors
           , "pages"  .= toJSON pages
           ]

  toJSON (ScrapeProgramPageError err) = toJSON err
  
instance Pretty ScrapeProgramPageResult where
  pretty (ScrapeProgramPageError err)
    =  "Error scraping program page: "
    <> pretty err

  pretty (ScrapeProgramPageSuccess errors courses)
    = "Program scraped! Successfully scraped "
    <> pretty (length courses)
    <> " and encoutered "
    <> pretty (length errors)
    <> " errors."
-}

type CourseScalpelRunner a = App a -> IO (Either Error a)

runCourseScalpel :: Config -> CourseScalpelRunner a
runCourseScalpel = runApp

mkConfig :: FilePath -> Config
mkConfig = Config

scrapeProgramPage
  :: forall m
  . (MonadCoursePage m,
     MonadProgramPage m,
     MonadError m)
  => Program
  -> m ProgramPage.ScrapeResult
scrapeProgramPage program =
  ProgramPage.scrapeProgramPage url
  {->>= \case    
    ProgramPage.ScrapeFail   err    -> pure $ ScrapeProgramPageError err
    ProgramPage.NetworkError errUrl -> undefined errUrl -- pure $ ScrapeProgramPageError     
    ProgramPage.ScrapeSuccess page -> do
      eCoursePages <- sequence $ parMap rpar CoursePage.scrapeCoursePage $
        ProgramPage.courseUrls page

      pure $ uncurry ScrapeProgramPageSuccess $
        partitionEithers eCoursePages
-}
  where
    url = Url $ "https://liu.se/studieinfo/program/"
          <> Program.slugToText (Program.slug program)
        
data ScrapeCoursePageResult
  = ScrapeCoursePageSuccess CoursePage
  | ScrapeCoursePageError   Error
  deriving Generic

instance ToJSON ScrapeCoursePageResult

instance Pretty ScrapeCoursePageResult where
  pretty (ScrapeCoursePageSuccess page)
    =  "Page: "
    <> pretty page
  pretty (ScrapeCoursePageError err)
    =  "Parse error: "
    <> pretty err

scrapeCoursePage
  :: forall m
  . (MonadCoursePage m,
     MonadError m)
  => Text
  -> m ScrapeCoursePageResult
scrapeCoursePage code =
  either ScrapeCoursePageError ScrapeCoursePageSuccess <$>
  CoursePage.scrapeCoursePage url
  where
    url = Url $"https://liu.se/studieinfo/kurs/" <> code
