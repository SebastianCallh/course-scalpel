{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module CourseScalpel.Course
  ( Course (..)
  , Area (..)
  , Level (..)
  , Specialization (..)
  , Credits (..)
  , Institution (..)
  , Field (..)
  , Prerequisites (..)
  , Examinator (..)
  , Content (..)
  , Subject (..)
  , Hours (..)
  ) where


import           Data.Aeson                    (FromJSON, ToJSON)
import           Data.Data                     (Typeable)
import           Data.Ord                      (comparing)
import           Data.Text                     (Text)
import           Data.Text.Prettyprint.Doc
import           GHC.Generics                  (Generic)
import           Test.QuickCheck               (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary,
                                                genericArbitrary)

import           CourseScalpel.Examination     (Credits (..), Examination)
import qualified CourseScalpel.Examination     as Examination
import           CourseScalpel.Time            (Hours (..))
import           CourseScalpel.Web             (Url)

--- Course ---

data Course = Course
  { code          :: !Text
  , name          :: !Text
  , level         :: !Level
  , areas         :: ![Area]
  , institution   :: !Institution
  , fields        :: ![Field]
  , prerequisites :: !(Maybe Prerequisites)
  , examinator    :: !(Maybe Examinator)
  , examinations  :: ![Examination]
  , content       :: !Content
  , subjects      :: ![Subject]
  , urls          :: ![Url]
  , selfStudyTime :: !Hours
  , scheduledTime :: !Hours
  } deriving (Show, Read, Typeable, Generic, FromJSON, ToJSON)

instance Eq Course where
  (==) a b = EQ == comparing code a b

instance Ord Course where
  (<=) a b = LT == comparing code a b

instance Pretty Course where
  pretty course@Course{..}
    = mconcat
    [ pretty code
    , ": "
    , pretty name
    , ", "
    , pretty $ credits course
    ]

credits :: Course -> Credits
credits =
  foldMap Examination.credits . examinations

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
  deriving (Show, Read, Eq, Ord, Typeable, Generic,FromJSON, ToJSON)

--- Content ---

newtype Content = Content { getContent :: Text }
  deriving (Show, Read, Eq, Ord, Typeable, Generic, FromJSON, ToJSON)

--- Examinator ---

newtype Examinator = Examinator { getExaminator :: Text }
    deriving (Show, Read, Eq, Ord, Typeable, Generic, FromJSON, ToJSON)

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
  deriving (Show, Read, Eq, Ord, Typeable, Generic, FromJSON, ToJSON)

--- Field ---

data Field
  = FieldHumanities
  | FieldLaw
  | FieldMedicine
  | FieldScience
  | FieldSociety
  | FieldTechnical
  deriving (Show, Read, Eq, Ord, Typeable, Generic, FromJSON, ToJSON)

--- Prerequisites ---

newtype Prerequisites = Prerequisites { getPrerequisites :: Text }
  deriving (Show, Read, Eq, Ord, Typeable, Generic, FromJSON, ToJSON)

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

--- Level ---

data Level
  = LevelG1
  | LevelG2
  | LevelA
  | LevelA1
  | LevelA2
  deriving (Show, Read, Eq, Ord, Typeable, Generic, FromJSON, ToJSON)

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
  deriving (Show, Eq, Ord, Typeable, Generic, ToADTArbitrary, FromJSON, ToJSON)

instance Arbitrary Specialization where
  arbitrary = genericArbitrary
