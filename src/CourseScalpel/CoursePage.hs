{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CourseScalpel.CoursePage
  ( CoursePage (..)
  , MonadCoursePage (..)
  , Header (..)
  , Plan (..)
  , Blocks (..)
  , Time (..)
  , Term (..)
  , couldNotParse
  , parseAreas
  , parseBlocks
  , parseExaminations
  , parseExamination
  , parseFields
  , parseGrading
  , parseSubjects
  , parseTime
  , parseUrls
  , parseInstitution
  , parseHeader
  , scrape
  , scraper
  , headerScraper
  , planScraper
  , toCourse
  ) where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Map                  (Map)
import qualified Data.Map                  as M
import           Data.Semigroup            ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc hiding (space)
import           GHC.Generics              (Generic)
import           Text.HTML.Scalpel         hiding (scrape)
import           Text.Megaparsec           hiding (parse)
import qualified Text.Megaparsec           as MP
import           Text.Megaparsec.Char

import           CourseScalpel.Course      (Course (..))
import qualified CourseScalpel.Course      as Course
import           CourseScalpel.Parsing     (takeMay)
import qualified CourseScalpel.Program     as Program
import           CourseScalpel.Web         (Url (..))

data CoursePage = CoursePage
  { coursePageHeader   :: !Header
  , coursePagePrograms :: ![Course.CourseProgram]
  , coursePagePlan     :: !Plan
  } deriving (Show, Eq)

-- | Needed for the studyinfo pin what version of the course to fetch.
data Term = HT | VT

class Monad m => MonadCoursePage m where
  scrapeCoursePage :: Url -> m CoursePage

data Error
  = ParseError Text Text
  | NetworkError Url
  | UnsupportedProgram Text
  deriving (Eq, Ord)

instance Show Error where
  show = showErrorComponent
  
type ParseResult = Either Error

scrape :: Url -> IO (ParseResult CoursePage)
scrape url =
  scrapeURL (T.unpack $ getUrl url) scraper >>= \case
    Nothing          -> pure . Left $ NetworkError url
    Just eCoursePage -> pure eCoursePage

type Parser = Parsec Error Text

instance ShowErrorComponent Error where
  showErrorComponent (ParseError txt typ) =
    T.unpack $ "Could not parse '" <> txt <> "' as " <> typ <> "."
  showErrorComponent (UnsupportedProgram slug) =
    T.unpack $ "No supported program for slug " <> slug <> "."
  showErrorComponent (NetworkError url) =
    T.unpack $ "Could not connect to " <> getUrl url <> "."

scraper :: Scraper Text (ParseResult CoursePage)
scraper = do
  courseHeader   <- headerScraper
  coursePrograms <- programsScraper
  coursePlan     <- planScraper
  pure $ CoursePage
    <$> courseHeader
    <*> coursePrograms
    <*> coursePlan

toCourse :: CoursePage ->  Course
toCourse CoursePage{..} = Course
  { courseCode          = headerCode        coursePageHeader
  , courseName          = headerName        coursePageHeader
  , courseCredits       = headerCredits     coursePageHeader
  , courseLevel         = planLevel         coursePagePlan
  , courseAreas         = planAreas         coursePagePlan
  , courseInstitution   = planInstitution   coursePagePlan
  , coursePrograms      =                   coursePagePrograms
  , courseFields        = planFields        coursePagePlan
  , coursePrerequisites = planPrerequisites coursePagePlan
  , courseGrades        = planGrades        coursePagePlan
  , courseExaminator    = planExaminator    coursePagePlan
  , courseExaminations  = planExaminations  coursePagePlan
  , courseContent       = planContent       coursePagePlan
  , courseSubjects      = planSubjects      coursePagePlan
  , courseSelfStudyTime = timeSelfStudy $   planTime          coursePagePlan
  , courseScheduledTime = timeScheduled $   planTime          coursePagePlan
  , courseUrls          = planUrls          coursePagePlan
  }

data Header = Header
  { headerCredits :: !Course.Credits
  , headerCode    :: !Text
  , headerName    :: !Text
  } deriving (Show, Eq)

{- A header can look like "Ingenjörsprofessionalism, del 1, 1 hp (TDDD70)"
   so by reversing and breaking on the comma (over here --^) the problem
   of parsing through the first comma is avoided. -}
parseHeader :: Text -> ParseResult Header
parseHeader x = do
  let (credCodePart, namePart) = T.breakOn "," $ T.reverse x
  let eCredCode = MP.parse credCodeParser  ""  $ T.reverse credCodePart
  let eName     = MP.parse nameParser      ""  $ T.reverse $ T.tail namePart
  let eHeader   = uncurry Header <$> eCredCode <*> eName
  either hoistError pure eHeader

  where
    hoistError :: MP.ParseError Char Error -> Either Error Header
    hoistError = const $ couldNotParse x "Course Page Header"

    credCodeParser :: Parser (Course.Credits, Text)
    credCodeParser = do
      _      <- space
      amount <- read <$> some digitChar
      _      <- string " hp "
      _      <- char '('
      code   <- T.pack <$> some alphaNumChar
      _      <- char ')'
      pure (Course.Credits amount, code)

    nameParser :: Parser Text
    nameParser = T.pack <$> some (alphaNumChar <|> spaceChar <|> char ',')

instance Pretty Header where
  pretty Header{..} =
    pretty headerCode <>
    ": "              <>
    pretty headerName <>
    ", "              <>
    pretty headerCredits

headerScraper :: Scraper Text (ParseResult Header)
headerScraper =
  chroot ("div" @: [hasClass "main-container"]) $ chroot "header" $
    parseHeader <$> text "h1"

{- Looks like the following so only the
   number in the beginning is relevant: "1 (HT 2018)". -}
parseSemester :: Text -> ParseResult Course.Semester
parseSemester x = parse' . T.strip $ T.takeWhile (not . (==) '(') x
  where
    parse' "1"  = pure Course.SemesterOne
    parse' "2"  = pure Course.SemesterTwo
    parse' "3"  = pure Course.SemesterThree
    parse' "4"  = pure Course.SemesterFour
    parse' "5"  = pure Course.SemesterFive
    parse' "6"  = pure Course.SemesterSix
    parse' "7"  = pure Course.SemesterSeven
    parse' "8"  = pure Course.SemesterEight
    parse' "9"  = pure Course.SemesterNine
    parse' "10" = pure Course.SemesterTen
    parse' _    = couldNotParse x "CoursePageSemester"

--- Area ---

parseAreas :: Text -> ParseResult [Course.Area]
parseAreas txt = parseNonEmpty txt "Areas" areas
  where
    areas = area `sepBy` char ','
    area  =
      string "Till\228mpad matematik"        *> pure Course.AreaAppliedMaths <|>
      string "Datavetenskap"                 *> pure Course.AreaComputerScience <|>
      string "Datateknik"                    *> pure Course.AreaComputerEngineering <|>
      string "Elektroteknik"                 *> pure Course.AreaElectrotechnic <|>
      string "Energi- och milj\246teknik"    *> pure Course.AreaEnergyEnvironment <|>
      string "Maskinteknik"                  *> pure Course.AreaEngineering <|>
      string "Informationsteknologi"         *> pure Course.AreaInformatics <|>
      string "Industriell ekonomi"           *> pure Course.AreaIndustrialEconomics <|>
      string "Matematik"                     *> pure Course.AreaMaths <|>
      string "Medicinsk teknik"              *> pure Course.AreaMedicinalEngineering <|>
      string "Fysik"                         *> pure Course.AreaPhysics <|>
      string "Produktutveckling"             *> pure Course.AreaProductDevelopment <|>
      string "Programmering"                 *> pure Course.AreaProgramming <|>
      string "Naturvetenskapliga omr\229det" *> pure Course.AreaScience <|>
      string "Teknik"                        *> pure Course.AreaTechnical <|>
      string "Teknisk fysik"                 *> pure Course.AreaTechnicalPhysics <|>
      string "Medieteknik"                   *> pure Course.AreaMediaEngineering <|>
      string "\214vriga \228mnen"            *> pure Course.AreaOther <|>
      string "se beslutade huvudområden"     *> pure Course.AreaOther

--- Blocks ---

newtype Blocks = Blocks { getBlocks :: [Course.Block] }
  deriving (Show, Eq)

{- | One course can span over several periods and
     one period can span several blocks, hence a list of
     list of blocks are parsed. -}
parseBlocks :: Text -> ParseResult [[Course.Block]]
parseBlocks txt = parseNonEmpty txt "Blocks" blocks
  where
    blocks = periodBlocks `sepBy` string ", "
    periodBlocks = do
      bs <- block `sepBy` char '/'
      if null bs
        then customFailure $ ParseError txt "Blocks"
        else pure bs        
    block  =
      string "0" *> pure Course.BlockNil   <|>
      string "1" *> pure Course.BlockOne   <|>
      string "2" *> pure Course.BlockTwo   <|>
      string "3" *> pure Course.BlockThree <|>
      string "4" *> pure Course.BlockFour  <|>
      string "-" *> pure Course.BlockNone 

--- Period ---

parsePeriods :: Text -> ParseResult [Course.Period]
parsePeriods = traverse (parsePeriod . T.strip) . T.splitOn ","

parsePeriod :: Text -> ParseResult Course.Period
parsePeriod "0" = pure Course.PeriodOne
parsePeriod "1" = pure Course.PeriodOne
parsePeriod "2" = pure Course.PeriodTwo
parsePeriod x   = couldNotParse x "CoursePagePeriod"

--- Importance ---

parseImportance :: Text -> ParseResult Course.Importance
parseImportance "v"   = pure Course.V
parseImportance "o"   = pure Course.O
parseImportance "f"   = pure Course.F
parseImportance "o/v" = pure Course.OV
parseImportance  x    = couldNotParse x "CoursePageImportance"

-- First table with class study-guide-table is the right one, currently.
-- Drops first tr since it is header. Very brittle.
programsScraper :: Scraper Text (ParseResult [Course.CourseProgram])
programsScraper =
  chroot ("table" @: [hasClass "study-guide-table"]) $
    sequenceA
    . filter supportedProgram
    . fmap parseCourseProgram
    . drop 1
    <$> innerHTMLs "tr"

  where
    supportedProgram :: ParseResult a -> Bool
    supportedProgram (Left (UnsupportedProgram _)) = False
    supportedProgram _                             = True

--- CourseProgram ---

parseCourseProgram :: Text -> ParseResult Course.CourseProgram
parseCourseProgram x = do
    let mFields    = scrapeStringLike x $ texts "td"
    let mSanFields = fmap sanitize <$> mFields
    case mSanFields >>= takeMay 8 of
        Nothing   -> Left $ ParseError x "Requires 8 td:s to be parsed."
        Just [ slugTxt, _, semesterTxt
             , periodTxt, blockTxt, _, _
             , importanceTxt
             ]  ->
          case programFromSlug slugTxt of
            Left _ -> Left $ UnsupportedProgram slugTxt
            Right  program ->
              let eCourseProgram = Course.CourseProgram program
                    <$> parseSemester semesterTxt
                    <*> parsePeriods periodTxt
                    <*> parseBlocks blockTxt
                    <*> parseImportance importanceTxt
              in case eCourseProgram of
                Left err            -> Left err
                Right courseProgram -> pure courseProgram

        _   -> Left $ ParseError x "Could not parse as CourseProgram."

  where
    programFromSlug :: Text ->  Either Text Program.Program
    programFromSlug = fmap Program.fromSlug . Program.parseSlug

--- Plan ---

data Plan = Plan
  { planAreas         :: ![Course.Area]
  , planInstitution   :: !Course.Institution
  , planFields        :: ![Course.Field]
  , planLevel         :: !Course.Level
  , planPrerequisites :: !(Maybe Course.Prerequisites)
  , planGrades        :: !Course.Grading
  , planExaminator    :: !(Maybe Course.Examinator)
  , planExaminations  :: ![Course.Examination]
  , planContent       :: !Course.Content
  , planSubjects      :: ![Course.Subject]
  , planUrls          :: ![Url]
  , planTime          :: !Time
  } deriving (Show, Eq)

planScraper :: Scraper Text (ParseResult Plan)
planScraper = do
  sections <- chroot ("section" @: [hasClass "studyguide-block"]) $
    chroots "div" $ do
      title   <- innerHTML "h3"
      content <- sanitize . snd . T.breakOnEnd "</h3>" <$> innerHTML "div"
      return (title, content)
  pure $ fromPageSections . M.fromList $ sections

  where
    fromPageSections :: Map Text Text -> ParseResult Plan
    fromPageSections sections = Plan
      <$> parseSection parseAreas         Nothing              "Huvudområde"
      <*> parseSection parseInstitution   Nothing              "Institution"
      <*> parseSection parseFields        Nothing              "Utbildningsområde"
      <*> parseSection parseLevel         Nothing              "Fördjupningsnivå"
      <*> parseSection parsePrerequisites (Just Nothing)       "Förkunskapskrav"
      <*> parseSection parseGrading       Nothing              "Betygsskala"
      <*> parseSection parseExaminator    (Just Nothing)       "Examinator"
      <*> parseSection parseExaminations  Nothing              "Examination"
      <*> parseSection parseContent       Nothing              "Kursinnehåll"
      <*> parseSection parseSubjects      (Just [])            "Ämnesområde"
      <*> parseSection parseUrls          (Just [])            "Kurshemsida och andra länkar"
      <*> parseSection parseTime          Nothing              "Undervisningstid"
      where
        parseSection :: (Text -> ParseResult a) -> Maybe a -> Text -> ParseResult a
        parseSection parse def key =
          case M.lookup key sections of
            Nothing      -> maybe (makeError key) pure  def
            Just section -> parse section

        makeError :: Text -> ParseResult a
        makeError secKey = couldNotParse
          ("Empty section " <> secKey) "CoursePagePlan"

--- Institution ---

parseInstitution :: Text -> ParseResult Course.Institution
parseInstitution = parseInstitution' . T.strip
  where
    parseInstitution' "Matematiska institutionen" =
      pure Course.InstitutionMAI
    parseInstitution' "Institutionen för Systemteknik" =
      pure Course.InstitutionISY

      -- typo in sh
    parseInstitution' "Institutionen f\246r  Datavetenskap" =
      pure Course.InstitutionIDA
    parseInstitution' "Institutionen f\246r Datavetenskap" =
      pure Course.InstitutionIDA
    parseInstitution' "Institutionen f\246r Medicinsk teknik" =
      pure Course.InstitutionMED

      -- typo in sh
    parseInstitution' "Institutionen f\246r Fysik, kemi och biologi" =
      pure Course.InstitutionIFM
    parseInstitution' "Institutionen f\246r  Fysik, kemi och biologi" =
      pure Course.InstitutionIFM
    parseInstitution' "Institutionen f\246r Ekonomisk och industriell utveckling"
      = pure Course.InstitutionIEE
    parseInstitution' "Institutionen f\246r Tema" =
      pure Course.InstitutionTema
    parseInstitution' "Institutionen för Teknik och naturvetenskap" =
      pure Course.InstitutionITN
    parseInstitution' "Tekniska fakultetskansliet" =
      pure Course.InstitutionTekFak
    parseInstitution' x = couldNotParse x "Institution"

--- Field ---

parseFields :: Text -> ParseResult [Course.Field]
parseFields = traverse (parseField . T.strip) . T.splitOn ","

parseField :: Text -> ParseResult Course.Field
parseField "Humanistiska omr\229det"             = pure Course.FieldHumanities
parseField "Medicinska omr\229det"               = pure Course.FieldMedicine
parseField "Tekniska omr\229det"                 = pure Course.FieldTechnical
parseField "Naturvetenskapliga omr\229det"       = pure Course.FieldScience
parseField "Samh\228llsvetenskapliga omr\229det" = pure Course.FieldSociety
parseField "Juridiska området"                   = pure Course.FieldLaw
parseField x                                     = couldNotParse x "Field"

--- Prerequisites ---

parsePrerequisites :: Text -> ParseResult (Maybe Course.Prerequisites)
parsePrerequisites = pure . pure . Course.Prerequisites

--- Subject ---
parseSubjects :: Text -> ParseResult [Course.Subject]
parseSubjects txt = do
  let x = MP.parse subjects "" txt
  either undefined pure x
  where
    subjects :: Parser [Course.Subject]
    subjects = subject `sepBy` char ','
    subject :: Parser Course.Subject
    subject =
      string "Datateknik"    *> pure Course.SubjectComputerScience <|> 
      string "Elektroteknik" *> pure Course.SubjectElectrotechnics <|>
      string "Milj\246v\229rd och milj\246skydd" *>
        pure Course.SubjectEnvironmentProtection <|>
      string "Engelska" *> pure Course.SubjectEnglish <|>
      string "Franska"  *> pure Course.SubjectFrench <|>
      string "Tyska" *> pure Course.SubjectGerman <|>
      string "Historia" *> pure Course.SubjectHistory <|>
      string "Informatik/Data- och systemvetenskap" *> pure Course.SubjectInformatics <|>
      string "Ledarskap, organisation och styrning" *>
        pure Course.SubjectLeadershipOrganisation <|>
      string "Ledarskap" *> pure Course.SubjectLeadership <|>
      string "Matematik" *> pure Course.SubjectMaths <|>
      string "Medie- o kommunikationsvetenskap" *> pure Course.SubjectMediaCommunication <|>
      string "Industriell ekonomi och organisation" *> pure Course.SubjectOrganisation <|>
      string "\214vrigt inom medicin"               *> pure Course.SubjectOtherMedicine <|>
      string "\214vriga tekniska \228mnen"          *> pure Course.SubjectOtherTechnical <|>
      string "Fysik"                                *> pure Course.SubjectPhysics <|>
      string "Filosofi"                             *> pure Course.SubjectPhilosophy <|>
      string "Spanska"                              *> pure Course.SubjectSpanish <|>
      string "Elektronik"                           *> pure Course.SubjectElectronics <|>
      string "Övriga ämnen"                         *> pure Course.SubjectOther <|>
      string "Juridik och rättsvetenskap"           *> pure Course.SubjectLaw <|>
      string "Medieproduktion"                      *> pure Course.SubjectMediaProduction
{-
      parseSubject :: Text -> ParseResult Course.Subject
      parseSubject "Datateknik"                           = pure Course.SubjectComputerScience
      parseSubject "Elektroteknik"                        = pure Course.SubjectElectrotechnics
      parseSubject "Milj\246v\229rd och milj\246skydd"    = pure Course.SubjectEnvironmentProtection
      parseSubject "Engelska"                             = pure Course.SubjectEnglish
      parseSubject "Franska"                              = pure Course.SubjectFrench
      parseSubject "Tyska"                                = pure Course.SubjectGerman
      parseSubject "Historia"                             = pure Course.SubjectHistory
      parseSubject "Informatik/Data- och systemvetenskap" = pure Course.SubjectInformatics
      parseSubject "Ledarskap, organisation och styrning" = pure Course.SubjectLeadershipOrganisation
      parseSubject "Ledarskap"                            = pure Course.SubjectLeadership
      parseSubject "Matematik"                            = pure Course.SubjectMaths
      parseSubject "Medie- o kommunikationsvetenskap"     = pure Course.SubjectMediaCommunication
      parseSubject "Industriell ekonomi och organisation" = pure Course.SubjectOrganisation
      parseSubject "\214vrigt inom medicin"               = pure Course.SubjectOtherMedicine
      parseSubject "\214vriga tekniska \228mnen"          = pure Course.SubjectOtherTechnical
      parseSubject "Fysik"                                = pure Course.SubjectPhysics
      parseSubject "Filosofi"                             = pure Course.SubjectPhilosophy
      parseSubject "Spanska"                              = pure Course.SubjectSpanish
      parseSubject "Elektronik"                           = pure Course.SubjectElectronics
      parseSubject "Övriga ämnen"                         = pure Course.SubjectOther
      parseSubject "Juridik och rättsvetenskap"           = pure Course.SubjectLaw
      parseSubject "Medieproduktion"                      = pure Course.SubjectMediaProduction
      parseSubject x = couldNotParse x "Subject"
-}

--- Time ---

-- | Slightly different from the other parsers as it
--   parses both self study and scheduled time. They are in the
--   same section in the DOM so this is the most convenient way.
data Time = Time
  { timeSelfStudy :: Word
  , timeScheduled :: Word
  } deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

parseTime :: Text -> ParseResult Time
parseTime x =  
  either (const $ couldNotParse x "Hours") pure $
  MP.parse parser "" (T.strip x)
    where
      parser :: Parser Time
      parser = do
        scheduled <- fmap read $ prelTxt
          *> optional (char '-')
          *> some digitChar
          <* string " h <br>"
        selfStudy <- fmap read $ recTxt
          *> optional (char '-')
          *> some digitChar
          <* string " h"
        pure Time
          { timeSelfStudy = selfStudy
          , timeScheduled = scheduled
          }

      prelTxt = string "Preliminary scheduled hours: " <|>
                string "Preliminär schemalagd tid: "
      recTxt  = string "Recommended self-study hours: " <|>
                string "Rekommenderad självstudietid: "        

--- Content ---

parseContent :: Text -> ParseResult Course.Content
parseContent = pure . Course.Content

--- Examinator ---

parseExaminator :: Text -> ParseResult (Maybe Course.Examinator)
parseExaminator = pure . pure . Course.Examinator

--- Examination ---

parseExaminations :: Text -> ParseResult [Course.Examination]
parseExaminations x =
    let mFieldList = scrapeStringLike x $
          chroot "table" $ innerHTMLs "tr"
    in case mFieldList of
      Nothing        -> couldNotParse x "Examinations"
      Just fieldList -> traverse parseExamination fieldList

parseExamination :: Text -> ParseResult Course.Examination
parseExamination x =
    let mFields = scrapeStringLike x $ texts "td"
    in case mFields >>= takeMay 4 of
      Nothing -> couldNotParse x "Examination"
      Just [code, description, grading, credits] ->
        Course.Examination code
          <$> parseExaminationType code
          <*> pure description
          <*> parseGrading grading
          <*> parseCredits credits
      Just _ ->  couldNotParse x "Examination"

--- ExaminationType ---

parseExaminationType :: Text -> ParseResult Course.ExaminationType
parseExaminationType x
  | "ANN"  `T.isPrefixOf` x = pure Course.ExaminationTypeANN
  | "AUSK" `T.isPrefixOf` x = pure Course.ExaminationTypeAUSK
  | "BAS"  `T.isPrefixOf` x = pure Course.ExaminationTypeBAS
  | "DAT"  `T.isPrefixOf` x = pure Course.ExaminationTypeDAT
  | "HEM"  `T.isPrefixOf` x = pure Course.ExaminationTypeHEM
  | "KTR"  `T.isPrefixOf` x = pure Course.ExaminationTypeKTR
  | "LAB"  `T.isPrefixOf` x = pure Course.ExaminationTypeLAB
  | "MOM"  `T.isPrefixOf` x = pure Course.ExaminationTypeMOM
  | "MUN"  `T.isPrefixOf` x = pure Course.ExaminationTypeMUN
  | "OPPO" `T.isPrefixOf` x = pure Course.ExaminationTypeOPPO
  | "PRA"  `T.isPrefixOf` x = pure Course.ExaminationTypePROJ
  | "TEN"  `T.isPrefixOf` x = pure Course.ExaminationTypeTEN
  | "UPG"  `T.isPrefixOf` x = pure Course.ExaminationTypeUPG
  | otherwise               = couldNotParse x "ExaminationType"

--- Grading ---

parseGrading :: Text -> ParseResult Course.Grading
parseGrading "U, 3, 4, 5"   = pure Course.GradingScale
parseGrading "U,3,4,5"      = pure Course.GradingScale
parseGrading "U, G"         = pure Course.GradingBinary
parseGrading "U,G"          = pure Course.GradingBinary
parseGrading "Deltagit (D)" = pure Course.GradingPresence
parseGrading "D"            = pure Course.GradingPresence
parseGrading ""             = pure Course.GradingUnspecified
parseGrading x              = couldNotParse x "Grading"

--- Credits ---

parseCredits :: Text -> ParseResult Course.Credits
parseCredits x =
  either errorOut mkCredit $ MP.parse parser "" $ T.strip x
  where
    errorOut = const $ couldNotParse x "Credits"
    mkCredit = pure . Course.Credits . read
    parser :: Parser String
    parser   = some floatChar <* optional (space *> string "hp")
    floatChar = digitChar <|> char '.'

--- Level ---

parseLevel :: Text -> ParseResult Course.Level
parseLevel "G1X" = pure Course.LevelG1
parseLevel "G2X" = pure Course.LevelG2
parseLevel "A"   = pure Course.LevelA
parseLevel "A1X" = pure Course.LevelA1
parseLevel "A2X" = pure Course.LevelA2
parseLevel x     = couldNotParse x "Level"

--- Url ---

parseUrls :: Text -> ParseResult [Url]
parseUrls x = do
  let mThing = scrapeStringLike x $ attrs "href" "a"
  let thing  = maybe [] (fmap Url) mThing :: [Url]
  pure thing

-- | Strips leading and trailing whitespace and removes
--   junk characters, typically from text inside tags.
sanitize :: Text -> Text
sanitize = T.strip . T.filter (not . isTrash)
  where isTrash = (`elem` ['\t', '\n', '\r'])

parseNonEmpty :: Text -> Text -> Parser [a] -> ParseResult [a]
parseNonEmpty txt typ parser =
  case MP.parse parser "" txt of
    Left  _  -> couldNotParse txt typ
    Right [] -> couldNotParse txt typ
    Right a  -> pure a

couldNotParse :: Text -> Text -> ParseResult a
couldNotParse txt typ = Left $ ParseError txt typ
