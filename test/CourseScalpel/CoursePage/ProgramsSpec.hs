module CourseScalpel.CoursePage.ProgramsSpec where

import           Test.Hspec

import qualified CourseScalpel.CoursePage.Programs as Programs
import           CourseScalpel.CourseProgram       (Block (..))
import qualified CourseScalpel.Parser              as Parser

spec :: SpecWith ()
spec =
    describe "parseBlocks" $ do
      it "parses all blocks correctly" $ do
        let input    = "0, 1, 2, 3, 4, -"
            expected = Right
              [ [BlockNil]
              , [BlockOne]
              , [BlockTwo]
              , [BlockThree]
              , [BlockFour]
              , [BlockNone]
              ]
        Programs.parseBlocks input `shouldBe` expected

      it "parses multiple blocks correctly" $ do
        let input    = "-, 2"
            actual   = Programs.parseBlocks input
            expected = Right
              [ [BlockNone]
              , [BlockTwo]
              ]
        actual `shouldBe` expected

      it "parses multiple blocks in one period correctly" $ do
        let input    = "3/4"
            actual   = Programs.parseBlocks input
            expected = Right
              [[ BlockThree
              , BlockFour
              ]]
        actual `shouldBe` expected

      it "fails to parse invalid block" $ do
        let input    = "not a block"
            expected = sequenceA [Parser.failure input "Blocks"]
            actual   = Programs.parseBlocks input
        actual `shouldBe` expected

