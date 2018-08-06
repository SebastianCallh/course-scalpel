module CourseScalpel.CoursePageSpec where

import qualified Data.Text                   as T
import           Data.Text.IO                (readFile)
import           Prelude                     hiding (readFile)
import           Test.Hspec
import           Text.HTML.Scalpel           (scrapeStringLike)

import qualified CourseScalpel.Course        as Course
import           CourseScalpel.CoursePage    (CoursePage (..))
import qualified CourseScalpel.CoursePage    as CoursePage
import           CourseScalpel.CourseProgram (Block (..), CourseProgram (..),
                                              Importance (..), Period (..),
                                              Semester (..))
import qualified CourseScalpel.CourseProgram as CourseProgram
import           CourseScalpel.Examination   (Examination (..))
import qualified CourseScalpel.Examination   as Examination
import qualified CourseScalpel.Program       as Program
import           CourseScalpel.Web           (Url (..))

spec :: SpecWith ()
spec =
  describe "coursePageScraper" $
    it "scrapes tata65 correctly" $ do
      markup <- readFile "test/markup/course-tata65.html"
      scrapeStringLike markup CoursePage.scraper `shouldBe` Just
        (Right CoursePage
         { CoursePage.header = CoursePage.Header
           { CoursePage.code    = "TATA65"
           , CoursePage.name    = "Diskret matematik"
           , CoursePage.credits = Course.Credits 6
           }
         , CoursePage.programs =
           CoursePage.Programs [ CourseProgram
             { CourseProgram.program = Program.Program
               { Program.code = Program.EngD
               , Program.slug = Program.P6CDDD
               }
             , CourseProgram.semester   = SemesterOne
             , CourseProgram.periods    = [PeriodOne, PeriodOne]
             , CourseProgram.blocks     = [[BlockNone], [BlockTwo]]
             , CourseProgram.importance = O
             }
           , CourseProgram
             { CourseProgram.program = Program.Program
               { Program.code = Program.EngU
               , Program.slug = Program.P6CMJU
               }
             , CourseProgram.semester   = SemesterOne
             , CourseProgram.periods    = [PeriodOne, PeriodOne]
             , CourseProgram.blocks     = [[BlockNone], [BlockTwo]]
             , CourseProgram.importance = O
             }
           ]
         , CoursePage.plan = CoursePage.Plan
           { CoursePage.areas         = [Course.AreaMaths, Course.AreaAppliedMaths]
           , CoursePage.institution   = Course.InstitutionMAI
           , CoursePage.fields        = [Course.FieldScience]
           , CoursePage.level         = Course.LevelG1
           , CoursePage.prerequisites = Just (Course.Prerequisites "prerequisites")
           , CoursePage.grades        = Examination.Scale
           , CoursePage.examinator    = Just (Course.Examinator "Carl Johan Casselgren")
           , CoursePage.examinations  =
             [ Examination
               { Examination.code = "TEN1"
               , Examination.typ = Examination.TEN
               , Examination.grading = Examination.Scale
               , Examination.description = "Skriftlig tentamen"
               , Examination.credits = Course.Credits 4
               }
             , Examination
               { Examination.code = "UPG1"
               , Examination.typ = Examination.UPG
               , Examination.grading = Examination.Binary
               , Examination.description = "Inlämningsuppgifter"
               , Examination.credits = Course.Credits 2
               }
             ]
           , CoursePage.content  = Course.Content "Course content"
           , CoursePage.subjects = [Course.SubjectMaths]
           , CoursePage.urls     = [Url "http://www.mai.liu.se/und/kurser/index-amne-tm.html"]
           , CoursePage.selfStudyTime  = Course.Time 80
           , CoursePage.scheduledTime  = Course.Time 80
           }
         })
