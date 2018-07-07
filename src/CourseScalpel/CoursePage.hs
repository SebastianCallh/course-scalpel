{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CourseScalpel.CoursePage
  ( CoursePage (..)
  , Header (..)
  , Plan (..)
  , Blocks (..)
  , Time (..)
  , parseAreas
  , parseArea
  , parseBlocks
  , parseBlock
  , parseExaminations
  , parseExamination
  , parseFields
  , parseGrading
  , parseTime
  , parseUrls
  , parseInstitution
  , scrape
  , scraper
  , headerScraper
  , planScraper
  , toCourse
  ) where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Char                 (isDigit)
import           Data.Data                 (Typeable)
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
import           CourseScalpel.Error       (HasError)
import           CourseScalpel.Parsing     (Parser, parseError, takeMay)
import qualified CourseScalpel.Program     as Program
import           CourseScalpel.Web         (Url (..), scrapeError)

data CoursePage = CoursePage
  { coursePageHeader   :: !Header
  , coursePagePrograms :: ![Course.CourseProgram]
  , coursePagePlan     :: !Plan
  } deriving (Show, Eq)

scrape :: forall m. (HasError m, MonadIO m) => Url -> m CoursePage
scrape url = do
  meCoursePage <- liftIO $ scrapeURL (T.unpack $ getUrl url) scraper
  maybe (scrapeError url "CoursePage") pure =<< sequence meCoursePage

scraper :: HasError m => Scraper Text (m CoursePage)
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
  { headerCode    :: !Text
  , headerName    :: !Text
  , headerCredits :: !Course.Credits
  } deriving (Show, Eq)

parseHeader :: HasError m => Text -> m Header
parseHeader x =
  either (const $ parseError x "CoursePageHeader") id $ MP.parse parser "" x
  where
    parser :: HasError m => Parser (m Header)
    parser = do
      name    <- T.pack <$> some (alphaNumChar <|> spaceChar)
      _       <- char ','
      credits <- parseCredits . T.pack <$> some (alphaNumChar <|> spaceChar)
      _       <- char '('
      code    <- T.pack <$> some alphaNumChar
      _       <- char ')'
      pure $ Header code name <$> credits

instance Pretty Header where
  pretty Header{..} =
    pretty headerCode <>
    ": "              <>
    pretty headerName <>
    ", "              <>
    pretty headerCredits

headerScraper :: HasError m => Scraper Text (m Header)
headerScraper =
  chroot ("div" @: [hasClass "main-container"]) $ chroot "header" $ do
    txt <- text "h1"
    pure $ parseHeader txt

--- Semester ---

--newtype Semester = Semester { getSemester :: Course.Semester }
--  deriving (Show, Eq)

{- Looks like the following so only the
   number in the beginning is relevant: "1 (HT 2018)". -}
parseSemester :: HasError m => Text -> m Course.Semester
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
    parse' _    = parseError x "CoursePageSemester"

--- Area ---

parseAreas :: HasError m => Text -> m [Course.Area]
parseAreas = traverse parseArea . T.splitOn ","

parseArea :: HasError m => Text -> m Course.Area
parseArea "Till\228mpad matematik"        = pure Course.AreaAppliedMaths
parseArea "Datavetenskap"                 = pure Course.AreaComputerScience
parseArea "Datateknik"                    = pure Course.AreaComputerEngineering
parseArea "Elektroteknik"                 = pure Course.AreaElectrotechnic
parseArea "Energi- och milj\246teknik"    = pure Course.AreaEnergyEnvironment
parseArea "Maskinteknik"                  = pure Course.AreaEngineering
parseArea "Informationsteknologi"         = pure Course.AreaInformatics
parseArea "Industriell ekonomi"           = pure Course.AreaIndustrialEconomics
parseArea "Matematik"                     = pure Course.AreaMaths
parseArea "Medicinsk teknik"              = pure Course.AreaMedicinalEngineering
parseArea "Fysik"                         = pure Course.AreaPhysics
parseArea "Produktutveckling"             = pure Course.AreaProductDevelopment
parseArea "Programmering"                 = pure Course.AreaProgramming
parseArea "Naturvetenskapliga omr\229det" = pure Course.AreaScience
parseArea "Teknik"                        = pure Course.AreaTechnical
parseArea "Teknisk fysik"                 = pure Course.AreaTechnicalPhysics
parseArea "Medieteknik"                   = pure Course.AreaMediaEngineering
parseArea "\214vriga \228mnen"            = pure Course.AreaOther
parseArea "se beslutade huvudområden"     = pure Course.AreaOther
parseArea x                               = parseError x "Area"

--- Blocks ---

newtype Blocks = Blocks { getBlocks :: [Course.Block] }
  deriving (Show, Eq) -- (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)

parseBlocks :: HasError m => Text -> m [Course.Block]
parseBlocks x =
    if null blockChars
    then parseError x "Block"
    else traverse parseBlock blockChars
    where
      blockChars = T.chunksOf 1 $ T.filter (\c -> isDigit c || c == '-') x

parseBlock :: HasError m => Text -> m Course.Block
parseBlock "1" = pure Course.BlockOne
parseBlock "2" = pure Course.BlockTwo
parseBlock "3" = pure Course.BlockThree
parseBlock "4" = pure Course.BlockFour
parseBlock "-" = pure Course.BlockNone
parseBlock  x  = parseError x "Block"

--- Period ---

parsePeriods :: HasError m => Text -> m [Course.Period]
parsePeriods = traverse (parsePeriod . T.strip) . T.splitOn ","

parsePeriod :: HasError m => Text -> m Course.Period
parsePeriod "0" = pure Course.PeriodOne
parsePeriod "1" = pure Course.PeriodOne
parsePeriod "2" = pure Course.PeriodTwo
parsePeriod x   = parseError x "CoursePagePeriod"

{-
newtype Periods = Periods { getPeriods :: [Period] }
  deriving (Show, Eq)

instance Parseable Periods where
  parse = fmap Periods . traverse (parse . T.strip) . T.splitOn ","

newtype Period = Period { getPeriod :: Course.Period }
  deriving (Show, Eq)

instance Parseable Period where
  parse "0" = pure $ Period Course.PeriodOne
  parse "1" = pure $ Period Course.PeriodOne
  parse "2" = pure $ Period Course.PeriodTwo
  parse x   = parseError x "CoursePagePeriod"
-}

--- Importance ---

parseImportance :: HasError m => Text -> m Course.Importance
parseImportance "v"   = pure Course.V
parseImportance "o"   = pure Course.O
parseImportance "f"   = pure Course.F
parseImportance "o/v" = pure Course.OV
parseImportance  x    = parseError x "CoursePageImportance"

--newtype Importance = Importance { getImportance :: Course.Importance }

--instance Parseable Importance where

-- First table with class study-guide-table is the right one, currently.
-- Drops first tr since it is header. Very brittle.
programsScraper :: forall m. HasError m => Scraper Text (m [Course.CourseProgram])
programsScraper =
  chroot ("table" @: [hasClass "study-guide-table"]) $ do
    txts <- drop 1 <$> innerHTMLs "tr"
    pure $ traverse parseCourseProgram txts

--- Program ---
{-
newtype Program = Program { getProgram :: Course.Program }
  deriving (Show, Eq)
-}

parseCourseProgram :: HasError m => Text -> m Course.CourseProgram
parseCourseProgram x = do
    let mFields    = scrapeStringLike x $ texts "td"
    let mSanFields = fmap sanitize <$> mFields
    case mSanFields >>= takeMay 8 of
        Nothing   -> parseError x "CoursePageProgram"
        Just [ slugTxt, _, semesterTxt
             , periodTxt, blockTxt, _, _
             , importanceTxt
             ]  -> Course.CourseProgram
                  <$> programFromSlug slugTxt
                  <*> (parseSemester semesterTxt)
                  <*> (parsePeriods periodTxt)
                  <*> (parseBlocks blockTxt)
                  <*> (parseImportance importanceTxt)
        _   -> parseError x "CoursePageProgram"
  where
    programFromSlug :: HasError m => Text ->  m Program.Program
    programFromSlug = fmap Program.fromSlug . Program.parseSlug


{-
instance Parseable Program where
  parse x = do
    let mFields    = scrapeStringLike x $ texts "td"
    let mSanFields = fmap sanitize <$> mFields
    case mSanFields >>= takeMay 8 of
        Nothing   -> parseError x "CoursePageProgram"
        Just [ codeTxt, _, semesterTxt
             , periodTxt, blockTxt, _, _
             , importanceTxt
             ]  -> fmap Program $ Course.Program
                  <$> parse codeTxt
                  <*> (getSemester <$> parse semesterTxt)
                  <*> (fmap getPeriod . getPeriods <$> parse periodTxt)
                  <*> (getBlocks <$> parse blockTxt)
                  <*> (getImportance <$> parse importanceTxt)
        _   -> parseError x "CoursePageProgram"
        -}

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

planScraper :: forall m. HasError m => Scraper Text (m Plan)
planScraper = do
  sections <- chroot ("section" @: [hasClass "studyguide-block"]) $
    chroots "div" $ do
      title   <- innerHTML "h3"
      content <- sanitize . snd . T.breakOnEnd "</h3>" <$> innerHTML "div"
      return (title, content)

  pure $ fromPageSections . M.fromList $ sections
  where
    fromPageSections :: Map Text Text -> m Plan
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
{-        parseField :: forall m a.
          (Typeable a, HasError m, Parseable a)
          => Maybe a
          -> Text
          -> m a
        parseField mDefault key =
          case M.lookup key sections of
            Nothing      -> maybe (makeError key) pure mDefault
            Just section -> parse section
-}
        parseSection :: (Text -> m a) -> Maybe a -> Text -> m a
        parseSection parse def key =
          case M.lookup key sections of
            Nothing      -> maybe (makeError key) pure  def
            Just section -> parse section

        makeError :: Text -> m a
        makeError secKey = parseError
          ("Empty section " <> secKey) "CoursePagePlan"

--- Institution ---

parseInstitution :: HasError m => Text -> m Course.Institution
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
    parseInstitution' x = parseError x "Institution"

--- Field ---

parseFields :: HasError m => Text -> m [Course.Field]
parseFields = traverse (parseField . T.strip) . T.splitOn ","

parseField :: HasError m => Text -> m Course.Field
parseField "Humanistiska omr\229det"             = pure Course.FieldHumanities
parseField "Medicinska omr\229det"               = pure Course.FieldMedicine
parseField "Tekniska omr\229det"                 = pure Course.FieldTechnical
parseField "Naturvetenskapliga omr\229det"       = pure Course.FieldScience
parseField "Samh\228llsvetenskapliga omr\229det" = pure Course.FieldSociety
parseField "Juridiska området"                   = pure Course.FieldLaw
parseField x                                     = parseError x "Field"

{-
newtype Fields = Fields { getFields :: [Field] }
  deriving (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)

instance Parseable Fields where
  parse = fmap Fields . traverse (parse . T.strip) . T.splitOn ","

newtype Field = Field { getField :: Course.Field }
  deriving (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)

instance Parseable Field where
-}

--- Prerequisites ---

parsePrerequisites :: HasError m => Text -> m (Maybe Course.Prerequisites)
parsePrerequisites = pure . pure . Course.Prerequisites

--- Subject ---
parseSubjects :: HasError m => Text -> m [Course.Subject]
parseSubjects = traverse parseSubject . T.splitOn ","

parseSubject :: HasError m => Text -> m Course.Subject
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
parseSubject x = parseError x "Subject"

{-
newtype Subject = Subject { getSubject :: Course.Subject }
  deriving (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)

newtype Subjects = Subjects { getSubjects :: [Subject] }
  deriving (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)

instance Parseable Subjects where
  parse = fmap Subjects . traverse parse . T.splitOn ","

instance Parseable Subject where
  parse "Datateknik"                           = pure Subject $ Course.SubjectComputerScience
  parse "Elektroteknik"                        = pure Subject $ Course.SubjectElectrotechnics
  parse "Milj\246v\229rd och milj\246skydd"    = pure Subject $ Course.SubjectEnvironmentProtection
  parse "Engelska"                             = pure Subject $ Course.SubjectEnglish
  parse "Franska"                              = pure Subject $ Course.SubjectFrench
  parse "Tyska"                                = pure Subject $ Course.SubjectGerman
  parse "Historia"                             = pure Subject $ Course.SubjectHistory
  parse "Informatik/Data- och systemvetenskap" = pure Subject $ Course.SubjectInformatics
  parse "Ledarskap, organisation och styrning" = pure Subject $ Course.SubjectLeadershipOrganisation
  parse "Ledarskap"                            = pure Subject $ Course.SubjectLeadership
  parse "Matematik"                            = pure Subject $ Course.SubjectMaths
  parse "Medie- o kommunikationsvetenskap"     = pure Subject $ Course.SubjectMediaCommunication
  parse "Industriell ekonomi och organisation" = pure Subject $ Course.SubjectOrganisation
  parse "\214vrigt inom medicin"               = pure Subject $ Course.SubjectOtherMedicine
  parse "\214vriga tekniska \228mnen"          = pure Subject $ Course.SubjectOtherTechnical
  parse "Fysik"                                = pure Subject $ Course.SubjectPhysics
  parse "Filosofi"                             = pure Subject $ Course.SubjectPhilosophy
  parse "Spanska"                              = pure Subject $ Course.SubjectSpanish
  parse "Elektronik"                           = pure Subject $ Course.SubjectElectronics
  parse "Övriga ämnen"                         = pure Subject $ Course.SubjectOther
  parse "Juridik och rättsvetenskap"           = pure Subject $ Course.SubjectLaw
  parse "Medieproduktion"                      = pure Subject $ Course.SubjectMediaProduction
  parse x = parseError x "Subject"
-}

--- Time ---

-- | Slightly different from the other parsers as it
--   parses both self study and scheduled time. They are in the
--   same section in the DOM so this is the most convenient way.
data Time = Time
  { timeSelfStudy :: Word
  , timeScheduled :: Word
  } deriving (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)

parseTime :: HasError m => Text -> m Time
parseTime x = either (const $ parseError x "Hours") pure $
    MP.parse parser "" (T.strip x)
    where
      parser :: Parser Time
      parser = do
        scheduled <- fmap read $ string "Preliminär schemalagd tid: "
          *> some digitChar
          <* string " h <br>"
        selfStudy <- fmap read $ string "Rekommenderad självstudietid: "
          *> some digitChar
          <* string " h"
        pure Time
          { timeSelfStudy = selfStudy
          , timeScheduled = scheduled
          }

--- Content ---

parseContent :: HasError m => Text -> m Course.Content
parseContent = pure . Course.Content

{-
newtype Content = Content { getContent :: Course.Content }
  deriving (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)

instance Parseable Content where
  parse =
-}

--- Examinator ---

parseExaminator :: HasError m => Text -> m (Maybe Course.Examinator)
parseExaminator = pure . pure . Course.Examinator

--- Examination ---

parseExaminations :: HasError m => Text -> m [Course.Examination]
parseExaminations x =
    let mFieldList = scrapeStringLike x $
          chroot "table" $ innerHTMLs "tr"
    in case mFieldList of
      Nothing        -> parseError x "Examinations"
      Just fieldList -> traverse parseExamination fieldList

parseExamination :: HasError m => Text -> m Course.Examination
parseExamination x =
    let mFields = scrapeStringLike x $ texts "td"
    in case mFields >>= takeMay 4 of
      Nothing -> parseError x "Examination"
      Just [code, description, grading, credits] ->
        Course.Examination code
          <$> parseExaminationType code
          <*> pure description
          <*> parseGrading grading
          <*> parseCredits credits
      Just _ ->  parseError x "Examination"
{-
newtype Examination = Examination { getExamination :: Course.Examination }
 deriving (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)

instance Parseable Examination where
  parse
-}
{-
newtype Examinations = Examinations { getExaminations :: [Examination] }
 deriving (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)

instance Parseable Examinations where
  parse
-}
--- ExaminationType ---

parseExaminationType :: HasError m => Text -> m Course.ExaminationType
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
  | otherwise               = parseError x "ExaminationType"

--- Grading ---

parseGrading :: HasError m => Text -> m Course.Grading
parseGrading "U, 3, 4, 5"   = pure Course.GradingScale
parseGrading "U,3,4,5"      = pure Course.GradingScale
parseGrading "U, G"         = pure Course.GradingBinary
parseGrading "U,G"          = pure Course.GradingBinary
parseGrading "Deltagit (D)" = pure Course.GradingPresence
parseGrading "D"            = pure Course.GradingPresence
parseGrading ""             = pure Course.GradingUnspecified
parseGrading x              = parseError x "Grading"

{-
newtype Grading = Grading { getGrading :: Course.Grading }
  deriving (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)

instance Parseable Grading where
-}

--- Credits ---

parseCredits :: HasError m => Text -> m Course.Credits
parseCredits x = either errorOut mkCredit $ MP.parse parser "" $ T.strip x
    where
      errorOut = const $ parseError x "Credits"
      mkCredit = pure . Course.Credits . read
      parser :: Parser String
      parser   = some floatChar <* optional (space *> string "hp")
      floatChar = digitChar <|> char '.'

{-
newtype Credits = Credits { getCredits :: Course.Credits }
  deriving (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)

instance Parseable Credits where
  parse
-}

--- Level ---

parseLevel :: HasError m => Text -> m Course.Level
parseLevel "G1X" = pure Course.LevelG1
parseLevel "G2X" = pure Course.LevelG2
parseLevel "A"   = pure Course.LevelA
parseLevel "A1X" = pure Course.LevelA1
parseLevel "A2X" = pure Course.LevelA2
parseLevel x     = parseError x "Level"

--- Url ---

parseUrls :: HasError m => Text -> m [Url]
parseUrls x = do
  let mThing = scrapeStringLike x $ attrs "href" "a"
  let thing  = maybe [] (fmap Url) mThing :: [Url]
  pure thing

-- | Strips leading and trailing whitespace and removes
--   junk characters, typically from text inside tags.
sanitize :: Text -> Text
sanitize = T.strip . T.filter (not . isTrash)
  where isTrash = (`elem` ['\t', '\n', '\r'])
