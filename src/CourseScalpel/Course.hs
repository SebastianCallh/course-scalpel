{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module CourseScalpel.Course where

import           Data.Aeson                    (FromJSON, ToJSON)
import           Data.Data                     (Typeable)
import           Data.Ord                      (comparing)
import           Data.Semigroup                (Semigroup (..), (<>))
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc     hiding (space)
import           Data.Word                     (Word)
import           GHC.Generics                  (Generic)
import           Test.QuickCheck               (Arbitrary, arbitrary)
import           Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary,
                                                genericArbitrary)

import           Text.Megaparsec               (optional, some, (<|>))
import qualified Text.Megaparsec               as MP

import           Text.Megaparsec.Char          (char, digitChar, space, string)

import           CourseScalpel.Parsing         (Parseable (..), Parser,
                                                parseError)
import           CourseScalpel.Program         (Program)

import           CourseScalpel.Web             (Url)

--- Course ---

data Course = Course
  { courseCode          :: !Text
  , courseName          :: !Text
  , courseCredits       :: !Credits
  , courseLevel         :: !Level
  , courseAreas         :: ![Area]
  , courseInstitution   :: !Institution
  , coursePrograms      :: ![CourseProgram]
  , courseFields        :: ![Field]
  , coursePrerequisites :: !(Maybe Prerequisites)
  , courseGrades        :: !Grading
  , courseExaminator    :: !(Maybe Examinator)
  , courseExaminations  :: ![Examination]
  , courseContent       :: !Content
  , courseSubjects      :: ![Subject]
  , courseUrls          :: ![Url]
  , courseSelfStudyTime :: !Word
  , courseScheduledTime :: !Word
  } deriving (Show, Read, Typeable, Generic, FromJSON, ToJSON)

instance Eq Course where
  (==) a b = EQ == comparing courseCode a b

instance Ord Course where
  (<=) a b = LT == comparing courseCode a b

instance Pretty Course where
  pretty Course{..}
    = mconcat
    [ pretty courseCode
    , ": "
    , pretty courseName
    , ", "
    , pretty courseCredits
    ]


--- Area ---

data Area
  = AreaAppliedMaths
  | AreaComputerScience
  | AreaComputerEngineering
  | AreaElectrotechnic
  | AreaEngineering
  | AreaEnergyEnvironment
  | AreaInformatics
  | AreaIndustrialEconomics
  | AreaMaths
  | AreaMedicinalEngineering
  | AreaMediaEngineering
  | AreaLaw
  | AreaPhysics
  | AreaProductDevelopment
  | AreaProgramming
  | AreaScience
  | AreaTechnical
  | AreaTechnicalPhysics
  | AreaOther
  deriving (Show, Read, Eq, Typeable, Generic,FromJSON, ToJSON)

--- Content ---

newtype Content = Content { getContent :: Text }
  deriving (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)

--- Time ---

-- | Slightly different from the other parsers as it
--   parses both self study and scheduled time. They are in the
--   same section in the DOM so this is the most convenient way.
data Time = Time
  { timeSelfStudy :: Word
  , timeScheduled :: Word
  } deriving (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)

--- Importance ---

data Importance = V | O | F | OV
  deriving (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)

--- Examinator ---

newtype Examinator = Examinator { getExaminator :: Text }
    deriving (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)

--- ExaminationType ---

data ExaminationType
  = ExaminationTypeTEN
  | ExaminationTypeLAB
  | ExaminationTypeUPG
  | ExaminationTypeAUSK
  | ExaminationTypeOPPO
  | ExaminationTypePROJ
  | ExaminationTypeKTR
  | ExaminationTypeMUN
  | ExaminationTypeANN
  | ExaminationTypeMOM
  | ExaminationTypeBAS
  | ExaminationTypeDAT
  | ExaminationTypeHEM
  deriving (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)

--- Examination ---

data Examination = Examination
  { examinationCode        :: !Text
  , examinationType        :: !ExaminationType
  , examinationDescription :: !Text
  , examinationGrading     :: !Grading
  , examinationCredits     :: !Credits
  } deriving (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)

--- Credits ---

newtype Credits = Credits { getCredits :: Float }
  deriving (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)

instance Parseable Credits where
  parse x = either errorOut mkCredit $ MP.parse parser "" $ T.strip x
    where
      errorOut = const $ parseError x "Credits"
      mkCredit = pure . Credits . read
      parser :: Parser String
      parser   = some floatChar <* optional (space *> string "hp")
      floatChar = digitChar <|> char '.'

instance Pretty Credits where
  pretty (Credits c) =
    pretty c <> " hp"

--- Subject ---

data Subject
  = SubjectComputerScience
  | SubjectElectronics
  | SubjectElectrotechnics
  | SubjectEnglish
  | SubjectEnvironmentProtection
  | SubjectFrench
  | SubjectGerman
  | SubjectHistory
  | SubjectInformatics
  | SubjectLaw
  | SubjectLeadership
  | SubjectLeadershipOrganisation
  | SubjectMaths
  | SubjectMediaCommunication
  | SubjectMediaProduction
  | SubjectOrganisation
  | SubjectOther
  | SubjectOtherMedicine
  | SubjectOtherTechnical
  | SubjectPhilosophy
  | SubjectPhysics
  | SubjectSpanish
  deriving (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)

--- Field ---

data Field
  = FieldHumanities
  | FieldLaw
  | FieldMedicine
  | FieldScience
  | FieldSociety
  | FieldTechnical
  deriving (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)

--- Prerequisites ---

newtype Prerequisites = Prerequisites { getPrerequisites :: Text }
  deriving (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)

--- Institution ---

data Institution
  = InstitutionMAI
  | InstitutionIDA
  | InstitutionISY
  | InstitutionMED
  | InstitutionIFM
  | InstitutionIEE
  | InstitutionITN
  | InstitutionTekFak
  | InstitutionTema
  deriving (Show, Read, Eq, Ord, Typeable, Generic, FromJSON, ToJSON)

--- Grading ---

data Grading
  = GradingBinary      -- U / G
  | GradingScale       -- U / 3 / 4 / 5
  | GradingPresence    -- Mandatory presence
  | GradingUnspecified -- There is a grading named "D". I do not know what it means.
  deriving (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)

{-
--- Validation ---

newtype ValidationError = ValidationError { getError :: [AppError] }

instance Semigroup ValidationError where
  (<>) x y = x : y -- ValidationError $ getError a <> "\n" <> getError b

validate :: Semigroup e => Either e (a -> b) -> Either e a -> Either e b
validate (Left a)  (Left b)  = Left $ a <> b
validate (Left a)  (Right _) = Left a
validate (Right _) (Left b)  = Left b
validate (Right f) (Right a) = Right $ f a

(-:) :: Semigroup e => Either e (a -> b) -> Either e a -> Either e b
(-:) = validate

valErr :: Text -> ValidationError
valErr = ValidationError
-}

--- Level ---

data Level
  = LevelG1
  | LevelG2
  | LevelA
  | LevelA1
  | LevelA2
  deriving (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)

--- Occasion ---

newtype Occasion = Occasion { getOccasion :: [Slot] }
  deriving (Show, Read, Eq, Ord, Typeable, Generic, FromJSON, ToJSON)

instance Semigroup Occasion where
  (<>) x y = Occasion $ getOccasion x <> getOccasion y

--- Slot ---

data Slot = Slot
  { slotSemester :: !Semester
  , slotPeriod   :: !Period
  , slotBlocks   :: ![Block]
  } deriving (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)

instance Ord Slot where
  compare a b =
    case comparing slotSemester a b of
      LT -> LT
      GT -> GT
      EQ -> case comparing slotPeriod a b of
        LT -> LT
        GT -> GT
        EQ -> comparing slotBlocks a b

--- Block ---

data Block
  = BlockNil
  | BlockOne
  | BlockTwo
  | BlockThree
  | BlockFour
  | BlockNone
  deriving (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)

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

data Period = PeriodOne | PeriodTwo
  deriving (Show, Read, Eq, Typeable, Generic, ToADTArbitrary, FromJSON, ToJSON)

instance Ord Period where
  compare PeriodOne PeriodTwo = LT
  compare PeriodOne PeriodOne = EQ
  compare PeriodTwo PeriodOne = GT
  compare PeriodTwo PeriodTwo = EQ

instance Arbitrary Period where
  arbitrary = genericArbitrary

instance Parseable Period where
  -- | Period 0 is for courses during nolle-p
  parse "Period 0" = pure PeriodOne
  parse "Period 1" = pure PeriodOne
  parse "Period 2" = pure PeriodTwo
  parse x          = parseError x "Period"

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
  deriving (Show, Read, Ord, Eq, Typeable, Generic, ToADTArbitrary, FromJSON, ToJSON)

instance Arbitrary Semester where
  arbitrary = genericArbitrary

instance Parseable Semester where
  parse "Termin 1"  = pure SemesterOne
  parse "Termin 2"  = pure SemesterTwo
  parse "Termin 3"  = pure SemesterThree
  parse "Termin 4"  = pure SemesterFour
  parse "Termin 5"  = pure SemesterFive
  parse "Termin 6"  = pure SemesterSix
  parse "Termin 7"  = pure SemesterSeven
  parse "Termin 8"  = pure SemesterEight
  parse "Termin 9"  = pure SemesterNine
  parse "Termin 10" = pure SemesterTen
  parse x           = parseError x "Semester"

--- Specialization ---

data Specialization
  = SpecializationAlgorithms
  | SpecializationCommunication
  | SpecializationComputerSystems
  | SpecializationElectronics
  | SpecializationGames
  | SpecializationIndustrialEconomics
  | SpecializationInternational
  | SpecializationMachineLearning
  | SpecializationMedicinalInformatics
  | SpecializationNone
  | SpecializationSafeSystems
  | SpecializationSignalProcessing
  | SpecializationSoftwareEngineering
  | SpecializationSystemsTechnology
  | SpecializationSystemOnChip
  deriving (Show, Eq, Typeable, Generic, ToADTArbitrary, FromJSON, ToJSON)

instance Arbitrary Specialization where
  arbitrary = genericArbitrary
{-
instance Parseable Specialization where
  parse "programmeringochalgoritmer"       = pure SpecializationAlgorithms
  parse "kommunikation"                    = pure SpecializationCommunication
  parse "datorsystem"                      = pure SpecializationComputerSystems
  parse "elektronik"                       = pure SpecializationElectronics
  parse "spelprogrammering"                = pure SpecializationGames
  parse "industriellekonomi"               = pure SpecializationIndustrialEconomics
  parse "internationalsoftwareengineering" = pure SpecializationInternational
  parse "aiochmaskininlärning"             = pure SpecializationMachineLearning
  parse "medicinskinformatik"              = pure SpecializationMedicinalInformatics
  parse "signal-ochbildbehandling"         = pure SpecializationSignalProcessing
  parse "säkrasystem"                      = pure SpecializationSafeSystems
  parse "storskaligmjukvaruutveckling"     = pure SpecializationSoftwareEngineering
  parse "systemteknologi"                  = pure SpecializationSystemsTechnology
  parse "system-on-chip"                   = pure SpecializationSystemOnChip
  parse "" = pure SpecializationNone
  parse x  = parseError x "Specialization"
-}

--- Program ---

data CourseProgram = CourseProgram
  { programProgram    :: !Program
  , programSemester   :: !Semester
  , programPeriods    :: ![Period]
  , programBlocks     :: ![[Block]]
  , programImportance :: !Importance
  } deriving (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)
