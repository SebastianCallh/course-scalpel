module CourseScalpel.CoursePage.HeaderSpec where

import           Test.Hspec

import           CourseScalpel.Course            (Credits (..))
import           CourseScalpel.CoursePage.Header (Header (..))
import qualified CourseScalpel.CoursePage.Header as Header

spec :: SpecWith ()
spec =
  describe "parseHeader" $ do
    it "parses course name with comma in it" $ do
      let input    = "Ingenjörsprofessionalism, del 1, 1 hp (TDDD70)"
          expected = Right Header
            { Header.credits = Credits 1
            , Header.code    = "TDDD70"
            , Header.name    = "Ingenjörsprofessionalism, del 1"
            }

          actual = Header.parse input
      actual `shouldBe` expected

    it "parses course name without comma in it" $ do
      let input    = "Diskret matematik, 6 hp (TATA65)"
          expected = Right Header
            { Header.credits = Credits 6
            , Header.code    = "TATA65"
            , Header.name    = "Diskret matematik"
            }

          actual   = Header.parse input
      actual `shouldBe` expected
