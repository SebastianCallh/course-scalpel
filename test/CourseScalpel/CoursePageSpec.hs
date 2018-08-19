module CourseScalpel.CoursePageSpec where

import           Data.Text.IO                      (readFile)
import           Prelude                           hiding (readFile)
import           Test.Hspec
import           Text.HTML.Scalpel                 (scrapeStringLike)

import           CourseScalpel.Course              (Course (..))
import qualified CourseScalpel.Course              as Course
import qualified CourseScalpel.CoursePage          as CoursePage
import           CourseScalpel.CoursePage.Occasion (Block (..), Importance (..),
                                                    Occasion (..), Period (..),
                                                    Semester (..))
import qualified CourseScalpel.CoursePage.Occasion as Occasion
import           CourseScalpel.Examination         (Examination (..))
import qualified CourseScalpel.Examination         as Examination
import qualified CourseScalpel.Program             as Program
import           CourseScalpel.Web                 (Url (..))

spec :: SpecWith ()
spec =
  describe "coursePageScraper" $
    it "scrapes tata65 correctly" $ do
      markup <- readFile "test/markup/course-tata65.html"
      let meCoursePage = scrapeStringLike markup CoursePage.scraper
      case meCoursePage of
        Nothing          -> expectationFailure "Scraper returned Nothing."
        Just eCoursePage -> case eCoursePage of
          Left err         -> expectationFailure $ show err
          Right coursePage -> do
            CoursePage.occasions coursePage `shouldBe`
              [ Occasion
                { Occasion.program = Program.Program
                  { Program.code = Program.EngD
                  , Program.slug = Program.P6CDDD
                  }
                , Occasion.semester   = SemesterOne
                , Occasion.periods    = [PeriodOne, PeriodOne]
                , Occasion.blocks     = [[BlockNone], [BlockTwo]]
                , Occasion.importance = O
                }
              , Occasion
                { Occasion.program = Program.Program
                  { Program.code = Program.EngU
                  , Program.slug = Program.P6CMJU
                  }
                , Occasion.semester   = SemesterOne
                , Occasion.periods    = [PeriodOne, PeriodOne]
                , Occasion.blocks     = [[BlockNone], [BlockTwo]]
                , Occasion.importance = O
                }
              ]

            CoursePage.course coursePage `shouldBe` Course
              { Course.code          = "TATA65"
              , Course.name          = "Diskret matematik"
              , Course.level         = Course.LevelG1
              , Course.areas         = [Course.AreaMaths, Course.AreaAppliedMaths]
              , Course.institution   = Course.InstitutionMAI
              , Course.fields        = [Course.FieldScience]
              , Course.prerequisites = Just (Course.Prerequisites "prerequisites")
              , Course.examinator    = Just (Course.Examinator "Carl Johan Casselgren")
              , Course.examinations  =
                [ Examination
                  { Examination.code        = "TEN1"
                  , Examination.typ         = Examination.TEN
                  , Examination.grading     = Examination.Scale
                  , Examination.description = "Skriftlig tentamen"
                  , Examination.credits     = Course.Credits 4
                  }
                , Examination
                  { Examination.code        = "UPG1"
                  , Examination.typ         = Examination.UPG
                  , Examination.grading     = Examination.Binary
                  , Examination.description = "Inlämningsuppgifter"
                  , Examination.credits     = Course.Credits 2
                  }
                ]
              , Course.content       = Course.Content "Course content"
              , Course.subjects      = [Course.SubjectMaths]
              , Course.selfStudyTime = Course.Hours 80
              , Course.scheduledTime = Course.Hours 80
              , Course.urls          =
                  [ Url "http://www.mai.liu.se/und/kurser/index-amne-tm.html"
                  ]
              }

