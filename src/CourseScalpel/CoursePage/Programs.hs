module CourseScalpel.CoursePage.Programs
  ( Programs (..)
  , scraper
  , parseBlocks
  ) where

import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Text.HTML.Scalpel
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           CourseScalpel.CourseProgram (Block (..), CourseProgram (..),
                                              Importance (..), Period (..),
                                              Semester (..))
import qualified CourseScalpel.Parser        as Parser
import           CourseScalpel.Program       (Program)
import qualified CourseScalpel.Program       as Program

newtype Programs = Programs { coursePrograms :: [CourseProgram] }
  deriving (Show, Eq)

-- First table with class study-guide-table is the right one, currently.
-- Drops first tr since it is header. Very brittle.
scraper :: Scraper Text (Parser.Result Programs)
scraper =
  chroot ("table" @: [hasClass "study-guide-table"]) $
    fmap (fmap Programs) $ sequenceA
    . filter supportedProgram
    . fmap parseCourseProgram
    . drop 1
    <$> innerHTMLs "tr"

  where
    supportedProgram :: Parser.Result a -> Bool
    supportedProgram (Left (Parser.UnsupportedProgram _)) = False
    supportedProgram _                                    = True

--- CourseProgram ---

parseCourseProgram :: Text -> Parser.Result CourseProgram
parseCourseProgram x = do
    let mFields    = scrapeStringLike x $ texts "td"
    let mSanFields = fmap Parser.sanitize <$> mFields
    case mSanFields >>= Parser.takeMay 8 of
        Nothing   -> Parser.failure x "Requires 8 td:s to be parsed."
        Just [ slugTxt, _, semesterTxt
             , periodTxt, blockTxt, _, _
             , importanceTxt
             ]  ->
          case programFromSlug slugTxt of
            Left _ -> Left $ Parser.UnsupportedProgram slugTxt
            Right  program ->
              let eCourseProgram = CourseProgram program
                    <$> parseSemester semesterTxt
                    <*> parsePeriods periodTxt
                    <*> parseBlocks blockTxt
                    <*> parseImportance importanceTxt
              in case eCourseProgram of
                Left err            -> Left err
                Right courseProgram -> pure courseProgram

        _   -> Parser.failure x "Could not parse as CourseProgram."

  where
    programFromSlug :: Text ->  Either Text Program
    programFromSlug = fmap Program.fromSlug . Program.parseSlug

{- Looks like the following so only the
   number in the beginning is relevant: "1 (HT 2018)". -}
parseSemester :: Text -> Parser.Result Semester
parseSemester x = parse' . T.strip $ T.takeWhile (not . (==) '(') x
  where
    parse' "1"  = pure SemesterOne
    parse' "2"  = pure SemesterTwo
    parse' "3"  = pure SemesterThree
    parse' "4"  = pure SemesterFour
    parse' "5"  = pure SemesterFive
    parse' "6"  = pure SemesterSix
    parse' "7"  = pure SemesterSeven
    parse' "8"  = pure SemesterEight
    parse' "9"  = pure SemesterNine
    parse' "10" = pure SemesterTen
    parse' _    = Parser.failure x "CoursePageSemester"

--- Blocks ---

{- | One course can span over several periods and
     one period can span several blocks, hence a list of
     list of blocks are parsed. -}
parseBlocks :: Text -> Parser.Result [[Block]]
parseBlocks txt = Parser.nonEmpty txt "Blocks" blocks
  where
    blocks = periodBlocks `sepBy` string ", "
    periodBlocks = do
      bs <- block `sepBy` char '/'
      if null bs
        then customFailure $ Parser.ParseError txt "Blocks"
        else pure bs
    block  =
      string "0" *> pure BlockNil   <|>
      string "1" *> pure BlockOne   <|>
      string "2" *> pure BlockTwo   <|>
      string "3" *> pure BlockThree <|>
      string "4" *> pure BlockFour  <|>
      string "-" *> pure BlockNone

--- Period ---

parsePeriods :: Text -> Parser.Result [Period]
parsePeriods = traverse (parsePeriod . T.strip) . T.splitOn ","

parsePeriod :: Text -> Parser.Result Period
parsePeriod "0" = pure PeriodOne
parsePeriod "1" = pure PeriodOne
parsePeriod "2" = pure PeriodTwo
parsePeriod x   = Parser.failure x "CoursePagePeriod"

--- Importance ---

parseImportance :: Text -> Parser.Result Importance
parseImportance "v"   = pure V
parseImportance "o"   = pure O
parseImportance "f"   = pure F
parseImportance "o/v" = pure OV
parseImportance  x    = Parser.failure x "CoursePageImportance"
