module CourseScalpel.CoursePage.OccasionSpec where

import           Test.Hspec

import           CourseScalpel.CoursePage.Occasion (Block (..))
import qualified CourseScalpel.CoursePage.Occasion as Occasion
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
        Occasion.parseBlocks input `shouldBe` expected

      it "parses multiple blocks correctly" $ do
        let input    = "-, 2"
            actual   = Occasion.parseBlocks input
            expected = Right
              [ [BlockNone]
              , [BlockTwo]
              ]
        actual `shouldBe` expected

      it "parses multiple blocks in one period correctly" $ do
        let input    = "3/4"
            actual   = Occasion.parseBlocks input
            expected = Right
              [[ BlockThree
              , BlockFour
              ]]
        actual `shouldBe` expected

      it "fails to parse invalid block" $ do
        let input    = "not a block"
            expected = sequenceA [Parser.parseError input "Blocks"]
            actual   = Occasion.parseBlocks input
        actual `shouldBe` expected

