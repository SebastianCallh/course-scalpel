module CourseScalpel.CoursePage.Occasion
  ( Occasion (..)
  , Semester (..)
  , Period (..)
  , Block (..)
  , Importance (..)
  , scraper
  , parseBlocks
  ) where

import           Data.Semigroup ((<>))
import           Data.Aeson                    (FromJSON, ToJSON)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc     (Pretty, pretty)
import           GHC.Generics                  (Generic)
import           Test.QuickCheck               (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary,
                                                genericArbitrary)
import           Text.HTML.Scalpel
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           CourseScalpel.Error          (unsupportedProgramError, parseError,
                                               filterUnsupportedProgramErrors)
import qualified CourseScalpel.Parser          as Parser
import           CourseScalpel.Program         (Program)
import qualified CourseScalpel.Program         as Program

data Occasion = Occasion
  { program    :: !Program
  , semester   :: !Semester
  , periods    :: ![Period]
  , blocks     :: ![[Block]]
  , importance :: !Importance
  } deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON Occasion
instance ToJSON Occasion

instance Pretty Occasion where
  pretty Occasion{..}
    =  pretty program
    <> pretty semester
    <> pretty periods
    <> pretty blocks
    <> pretty importance
    
-- First table with class study-guide-table is the right one, currently.
-- Drops first tr since it is header. Very brittle.
scraper :: Scraper Text (Parser.Result [Occasion])
scraper =
  chroot ("table" @: [hasClass "study-guide-table"]) $
    sequenceA
    . filterUnsupportedProgramErrors
    . fmap parseOccasion
    . drop 1
    <$> innerHTMLs "tr"

--- Occasion ---

parseOccasion :: Text -> Parser.Result Occasion
parseOccasion x = do
    let mFields    = scrapeStringLike x $ texts "td"
    let mSanFields = fmap Parser.sanitize <$> mFields
    case mSanFields >>= Parser.takeMay 8 of
        Nothing   -> parseError x "Requires 8 td:s to be parsed."
        Just [ slugTxt, _, semesterTxt
             , periodTxt, blockTxt, _, _
             , importanceTxt
             ]  ->
          case programFromSlug slugTxt of
            Left _ -> unsupportedProgramError slugTxt
            Right  program ->
              let eOccasion = Occasion program
                    <$> parseSemester semesterTxt
                    <*> parsePeriods periodTxt
                    <*> parseBlocks blockTxt
                    <*> parseImportance importanceTxt
              in case eOccasion of
                Left err            -> Left err
                Right courseProgram -> pure courseProgram

        _   -> parseError x "Could not parse as Occasion."

  where
    programFromSlug :: Text ->  Either Text Program
    programFromSlug = fmap Program.fromSlug . Program.parseSlug

--- Importance ---

data Importance = V | O | F | OV
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON Importance
instance ToJSON Importance

instance Pretty Importance where
  pretty V  = "Valbar"
  pretty O  = "Obligatorisk"
  pretty F  = "Frivillig"
  pretty OV = "Obligatorisk/Valbad"
  
--- Semester ---

data Semester
  = SemesterOne
  | SemesterTwo
  | SemesterThree
  | SemesterFour
  | SemesterFive
  | SemesterSix
  | SemesterSeven
  | SemesterEight
  | SemesterNine
  | SemesterTen
  deriving (Show, Read, Ord, Eq, Generic)

instance ToADTArbitrary Semester
instance FromJSON Semester
instance ToJSON Semester

instance Arbitrary Semester where
  arbitrary = genericArbitrary

instance Pretty Semester where
  pretty SemesterOne   = "1"
  pretty SemesterTwo   = "2"
  pretty SemesterThree = "3"
  pretty SemesterFour  = "4"
  pretty SemesterFive  = "5"
  pretty SemesterSix   = "6"
  pretty SemesterSeven = "7"
  pretty SemesterEight = "8"
  pretty SemesterNine  = "9"
  pretty SemesterTen   = "10"

--- Period ---

data Period = PeriodOne | PeriodTwo
  deriving (Show, Read, Eq, Generic)

instance ToADTArbitrary Period
instance FromJSON Period
instance ToJSON Period

instance Pretty Period where
  pretty PeriodOne = "1"
  pretty PeriodTwo = "2"
  
instance Ord Period where
  compare PeriodOne PeriodTwo = LT
  compare PeriodOne PeriodOne = EQ
  compare PeriodTwo PeriodOne = GT
  compare PeriodTwo PeriodTwo = EQ

instance Arbitrary Period where
  arbitrary = genericArbitrary

--- Block ---

data Block
  = BlockNil
  | BlockOne
  | BlockTwo
  | BlockThree
  | BlockFour
  | BlockNone
  deriving (Show, Read, Eq, Generic)

instance ToADTArbitrary Block
instance FromJSON Block
instance ToJSON Block

instance Pretty Block where
  pretty BlockNil   = "0"
  pretty BlockOne   = "1"
  pretty BlockTwo   = "2"
  pretty BlockThree = "3"
  pretty BlockFour  = "3"
  pretty BlockNone  = "4"
  
instance Ord Block where
  compare BlockNil BlockNone    = GT
  compare BlockNil BlockNil     = EQ
  compare BlockNil BlockOne     = LT
  compare BlockNil BlockTwo     = LT
  compare BlockNil BlockThree   = LT
  compare BlockNil BlockFour    = LT
  compare BlockOne BlockNone    = GT
  compare BlockOne BlockNil     = GT
  compare BlockOne BlockOne     = EQ
  compare BlockOne BlockTwo     = LT
  compare BlockOne BlockThree   = LT
  compare BlockOne BlockFour    = LT
  compare BlockTwo BlockNone    = GT
  compare BlockTwo BlockNil     = GT
  compare BlockTwo BlockOne     = GT
  compare BlockTwo BlockTwo     = EQ
  compare BlockTwo BlockThree   = LT
  compare BlockTwo BlockFour    = LT
  compare BlockThree BlockNone  = GT
  compare BlockThree BlockNil   = GT
  compare BlockThree BlockOne   = GT
  compare BlockThree BlockTwo   = GT
  compare BlockThree BlockThree = EQ
  compare BlockThree BlockFour  = LT
  compare BlockFour BlockNone   = GT
  compare BlockFour BlockNil    = GT
  compare BlockFour BlockOne    = GT
  compare BlockFour BlockTwo    = GT
  compare BlockFour BlockThree  = GT
  compare BlockFour BlockFour   = EQ
  compare BlockNone BlockNone   = EQ
  compare BlockNone BlockNil    = LT
  compare BlockNone BlockOne    = GT
  compare BlockNone BlockTwo    = GT
  compare BlockNone BlockThree  = GT
  compare BlockNone BlockFour   = GT

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
    parse' _    = parseError x "CoursePageSemester"

--- Blocks ---

--  either (const $ parseError x "Header") pure $
--    MP.parse parser "" $ T.strip x

{- | One course can span over several periods and
     one period can span several blocks, hence a list of
     list of blocks are parsed. -}
parseBlocks :: Text -> Parser.Result [[Block]]
parseBlocks txt = either Left pure $
  Parser.nonEmpty txt "Blocks" blocks
  where
    blocks = periodBlocks `sepBy` string ", "
    periodBlocks = do
      bs <- block `sepBy` char '/'
      if null bs
        then fail "No blocks parsed." -- customFailure $ Parser.ParseError txt "Blocks"
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
parsePeriod x   = parseError x "CoursePagePeriod"

--- Importance ---

parseImportance :: Text -> Parser.Result Importance
parseImportance "v"   = pure V
parseImportance "o"   = pure O
parseImportance "f"   = pure F
parseImportance "o/v" = pure OV
parseImportance  x    = parseError x "CoursePageImportance"
