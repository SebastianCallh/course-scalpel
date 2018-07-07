module CourseScalpel.ListCourseSpec where

import           Data.Either              (lefts, rights)
import           Data.List                (nub)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Test.Hspec
import           Test.QuickCheck
import           Text.HTML.Scalpel        (attrs, hasClass, scrapeStringLike,
                                           (@:))

import           CourseScalpel.ListCourse (ListCourse (..))
import qualified CourseScalpel.ListCourse as ListCourse
import qualified CourseScalpel.PageCourse as PageCourse
import           CourseScalpel.Types

spec :: SpecWith ()
spec =
  describe "scraper" $ do
    it "scrapes program 6cddd correctly" $ do
      markup <- T.pack <$> readFile "markup/program-6cddd.html"
      let meCourses = scrapeStringLike markup $ ListCourse.scraper engD
      let mUniqueCodes = fmap nub $ scrapeStringLike markup $
            attrs "data-course-code" ("tr" @: [hasClass "main-row"])

      filter (not . isKnownError) . lefts  <$> meCourses `shouldBe` pure []
      length <$> meCourses `shouldBe` pure 235 -- length <$> mUniqueCodes

    it "scrapes course that has occasions in different semesters" $ property $ do
      markup  <- T.pack <$> readFile "markup/list-course-different-semesters.html"
      let scraper = ListCourse.scraper engD

      let mCourses = scrapeStringLike markup scraper
      mCourses `shouldBe` pure
        [pure ListCourse
          { listCourseProgram   = engD
          , listCourseSpecializations = [SpecializationNone]
          , listCourseCode      = "TFMT13"
          , listCourseName      = "Mätteknik"
          , listCourseCredits   = Credits 4
          , listCourseLevel     = LevelG1
          , listCourseOccasions = [ Occasion [Slot SemesterFive  PeriodOne BlockTwo]
                                  , Occasion [Slot SemesterSeven PeriodOne BlockOne]
                                  ]
          , listCourseUrl        = Url "https://liu.se/studieinfo/kurs/tfmt13"
          , listCourseImportance = V
          }
        ]
