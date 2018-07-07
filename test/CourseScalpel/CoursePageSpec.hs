{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module CourseScalpel.CoursePageSpec where

import           Control.Monad.Except     (ExceptT, MonadError, runExceptT)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Maybe               (isJust)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.IO             (readFile)
import           Prelude                  hiding (readFile)
import           Test.Hspec
import           Text.HTML.Scalpel        (scrapeStringLike)

import           CourseScalpel.Course     (CourseProgram (..))
import qualified CourseScalpel.Course     as Course
import           CourseScalpel.CoursePage (Blocks (..), CoursePage (..))
import qualified CourseScalpel.CoursePage as CoursePage
import           CourseScalpel.Error      (AppError)
import           CourseScalpel.Parsing    (parseError)
import           CourseScalpel.Parsing    (parse)
import           CourseScalpel.Program    (Program (..))
import qualified CourseScalpel.Program    as Program
import           CourseScalpel.Web        (Url (..))

spec :: SpecWith ()
spec =
  describe "coursePageScraper" $ do
    it "scrapes tata65 correctly" $ do
      markup <- readFile "markup/course-tata65.html"
      scrapeStringLike markup CoursePage.scraper `shouldBe` Just
        (Right CoursePage
         { coursePageHeader = CoursePage.Header
           { CoursePage.headerCode    = "TATA65"
           , CoursePage.headerName    = "Diskret matematik"
           , CoursePage.headerCredits = Course.Credits 6
           }
         , coursePagePrograms =
           [ Course.CourseProgram
             { Course.programProgram = Program.Program
               { Program.programCode = Program.EngD
               , Program.programSlug = Program.P6CDDD
               }
             , Course.programSemester   = Course.SemesterOne
             , Course.programPeriods    = [Course.PeriodOne, Course.PeriodOne]
             , Course.programBlocks     = [Course.BlockNone, Course.BlockTwo]
             , Course.programImportance = Course.O
             }
           , Course.CourseProgram
             { Course.programProgram = Program.Program
               { Program.programCode = Program.EngU
               , Program.programSlug = Program.P6CMJU
               }
             , Course.programSemester   = Course.SemesterOne
             , Course.programPeriods    = [Course.PeriodOne, Course.PeriodOne]
             , Course.programBlocks     = [Course.BlockNone, Course.BlockTwo]
             , Course.programImportance = Course.O
             }
           ]
         , coursePagePlan = CoursePage.Plan
           { CoursePage.planAreas         = [Course.AreaMaths, Course.AreaAppliedMaths]
           , CoursePage.planInstitution   = Course.InstitutionMAI
           , CoursePage.planFields        = [Course.FieldScience]
           , CoursePage.planLevel         = Course.LevelG1
           , CoursePage.planPrerequisites = Just (Course.Prerequisites "prerequisites")
           , CoursePage.planGrades        = Course.GradingScale
           , CoursePage.planExaminator    = Just (Course.Examinator "Carl Johan Casselgren")
           , CoursePage.planExaminations  =
             [ Course.Examination
               { examinationCode = "TEN1"
               , examinationType = Course.ExaminationTypeTEN
               , examinationGrading = Course.GradingScale
               , examinationDescription = "Skriftlig tentamen"
               , examinationCredits = Course.Credits 4
               }
             , Course.Examination
               { examinationCode = "UPG1"
               , examinationType = Course.ExaminationTypeUPG
               , examinationGrading = Course.GradingBinary
               , examinationDescription = "Inlämningsuppgifter"
               , examinationCredits = Course.Credits 2
               }
             ]
           , planContent       = Course.Content "Course content"
           , planSubjects      = [Course.SubjectMaths]
           , planUrls          = [Url "http://www.mai.liu.se/und/kurser/index-amne-tm.html"]
           , planTime          = CoursePage.Time 80 80
           }
         })


    describe "parseBlocks" $ do
      it "parses single blocks correctly" $ do
        let input    = ["1", "2", "3", "4", "-"]
            expected = Right $
              [ Course.BlockOne
              , Course.BlockTwo
              , Course.BlockThree
              , Course.BlockFour
              , Course.BlockNone
              ]

        traverse CoursePage.parseBlock input `shouldBe` expected

      it "parses multiple blocks correctly" $ do
        let input    = "3/4"
            actual   = CoursePage.parseBlocks input :: Either AppError [Course.Block]
            expected = Right
              [ Course.BlockThree
              , Course.BlockFour
              ]
        actual `shouldBe` expected

    describe "parseArea" $ do
      it "parses valid values" $ do
        let input = T.intercalate ","
              [ "Till\228mpad matematik"
              , "Datavetenskap"
              , "Datateknik"
              , "Elektroteknik"
              , "Informationsteknologi"
              , "Matematik"
              , "Medicinsk teknik"
              , "Fysik"
              , "Naturvetenskapliga omr\229det"
              , "Teknik"
              , "Teknisk fysik"
              , "\214vriga \228mnen"
              ]

            expected = Right
              [ Course.AreaAppliedMaths
              , Course.AreaComputerScience
              , Course.AreaComputerEngineering
              , Course.AreaElectrotechnic
              , Course.AreaInformatics
              , Course.AreaMaths
              , Course.AreaMedicinalEngineering
              , Course.AreaPhysics
              , Course.AreaScience
              , Course.AreaTechnical
              , Course.AreaTechnicalPhysics
              , Course.AreaOther
              ]
            actual = CoursePage.parseAreas input :: Either AppError [Course.Area]
        actual `shouldBe` expected

      it "fails to parse non-valid values" $ do
        let input    = "not an area"
            actual   = CoursePage.parseArea input :: Either AppError Course.Area
            expected = parseError input "Area"
        actual `shouldBe` expected

    describe "parseExaminations" $ do
      it "parses expected markup" $ do
        input <- readFile "markup/examination.html"
        let actual   = CoursePage.parseExaminations input
            expected = Right
              [ Course.Examination "TEN1"
                Course.ExaminationTypeTEN  "Skriftlig tentamen"
                Course.GradingScale
                (Course.Credits 4)
              , Course.Examination "UPG1"
                Course.ExaminationTypeUPG  "Uppgifter"
                Course.GradingBinary
                (Course.Credits 28)
              , Course.Examination "OPPO"
                Course.ExaminationTypeOPPO "Opponering"
                Course.GradingBinary
                (Course.Credits 2)
              , Course.Examination "AUSK"
                Course.ExaminationTypeAUSK "Auskonsultation"
                Course.GradingPresence
                (Course.Credits 2)
              , Course.Examination "PRA1"
                Course.ExaminationTypePROJ "Project"
                Course.GradingPresence
                (Course.Credits 1.5)
              , Course.Examination "DAT1"
                Course.ExaminationTypeDAT  "Datortentamen"
                Course.GradingBinary
                (Course.Credits 5)
              , Course.Examination "HEM2"
                Course.ExaminationTypeHEM  "Hemtentamen"
                Course.GradingScale
                (Course.Credits 6)
              ]

        actual`shouldBe` expected

      it "failes to parse" $ do
        let input    = "not an examination"
            actual   = CoursePage.parseExamination input :: Either AppError Course.Examination
            expected = parseError input "Examination"
        actual `shouldBe` expected

      it "parses expected markup" $ do
        input <- readFile "markup/examination-tams22.html"
        let expected1 = Course.Examination
              { examinationCode        = "TEN1"
              , examinationType        = Course.ExaminationTypeTEN
              , examinationDescription = "En skriftlig tentamen (U,3,4,5)"
              , examinationGrading     = Course.GradingScale
              , examinationCredits     = Course.Credits 5
              }

            expected2 = Course.Examination
              { examinationCode        = "LAB1"
              , examinationType        = Course.ExaminationTypeLAB
              , examinationDescription = "Obligatoriska inlämningsuppgifter (U,3,4,5)"
              , examinationGrading     = Course.GradingScale
              , examinationCredits     = Course.Credits 1
              }

            actual = CoursePage.parseExaminations input
        actual `shouldBe` Right [expected1, expected2]

    describe "parseUrls" $ do
      it "parses an url" $ do
        input <- readFile "markup/url.html"
        let expected = Right [Url "http://www.isy.liu.se/en/edu/kurs/TSTE12/"]
            actual   = CoursePage.parseUrls input
        actual `shouldBe` expected

      it "returns empty list on no urls" $ do
        let input    = ""
            expected = Right []
            actual   = CoursePage.parseUrls input :: Either AppError [Url]
        actual `shouldBe` expected

    describe "parseFields" $ do
      it "parses expected input" $ do
        let input = T.intercalate ", "
              [ "Humanistiska omr\229det"
              , "Medicinska omr\229det"
              , "Tekniska omr\229det"
              , "Naturvetenskapliga omr\229det"
              , "Samh\228llsvetenskapliga omr\229det"
              ]

            expected = Right
              [ Course.FieldHumanities
              , Course.FieldMedicine
              , Course.FieldTechnical
              , Course.FieldScience
              , Course.FieldSociety
              ]

            actual = CoursePage.parseFields input
        actual `shouldBe` expected

      it "fails to parse illegal values" $ do
        let input    = "not a field"
            expected = sequence [parseError input "Field"]
            actual   = CoursePage.parseFields input :: Either AppError [Course.Field]
        actual `shouldBe` expected

    describe "parseBlock" $ do
      it "fails to parse invalid block" $ do
        let input    = "not a block"
            expected = sequence [parseError input "Block"]
            actual   = CoursePage.parseBlocks input :: Either AppError [Course.Block]
        actual `shouldBe` expected

    describe "parseInstitution" $ do
      it "parses valid values" $ do
        let input =
              [ "Matematiska institutionen"
              , "Institutionen för Systemteknik"
              , "Institutionen f\246r  Datavetenskap"
              , "Institutionen f\246r Datavetenskap"
              , "Institutionen f\246r Medicinsk teknik"
              , "Institutionen f\246r Fysik, kemi och biologi"
              , "Institutionen f\246r  Fysik, kemi och biologi" -- Typo on studybook
              , "Institutionen f\246r Ekonomisk och industriell utveckling"
              , "Tekniska fakultetskansliet"
              ]

            expected = Right
              [ Course.InstitutionMAI
              , Course.InstitutionISY
              , Course.InstitutionIDA
              , Course.InstitutionIDA
              , Course.InstitutionMED
              , Course.InstitutionIFM
              , Course.InstitutionIFM
              , Course.InstitutionIEE
              , Course.InstitutionTekFak
              ]

            actual = traverse CoursePage.parseInstitution input
        actual `shouldBe` expected

      it "parses white space padded values" $ do
        let input    = "Institutionen f\246r Fysik, kemi och biologi "
            expected = Right Course.InstitutionIFM
            actual   = CoursePage.parseInstitution input
        actual `shouldBe` expected

      it "fails to parse non-valid values" $ do
        let input    = "not an institution"
            expected = parseError input "Institution"
            actual   = CoursePage.parseInstitution input :: Either AppError Course.Institution
        actual `shouldBe` expected


    describe "parseGrading" $ do
      it "parses valid values" $ do
        let input    = ["U, G", "U,G", "U, 3, 4, 5", "U,3,4,5", "Deltagit (D)", "D", ""]
            expected = Right
              [ Course.GradingBinary
              , Course.GradingBinary
              , Course.GradingScale
              , Course.GradingScale
              , Course.GradingPresence
              , Course.GradingPresence
              , Course.GradingUnspecified
              ]
            actual = traverse CoursePage.parseGrading input

        actual `shouldBe` expected

      it "fails to parse non-valid values" $ do
        let input    = "not a gradescale"
            expected = parseError input "Grading"
            actual   = CoursePage.parseGrading input :: Either AppError Course.Grading
        actual `shouldBe` expected

    describe "parseCredits" $ do
      it "parses integer" $
        parse "5" `shouldBe` Right (Course.Credits 5)

      it "parses integer with asterisk" $
        parse "5*" `shouldBe` Right (Course.Credits 5)

      it "parses integer will trailing hp" $
        parse "5 hp" `shouldBe` Right (Course.Credits 5)

      it "parses decimal with trailing hp" $
        parse "1.5 hp" `shouldBe` Right (Course.Credits 1.5)

      it "fails to parse non-numeric value" $ do
        let input = "not credit"
        (parse input :: Either AppError Course.Credits) `shouldBe` parseError input "Credits"

    describe "parse CourseTime" $ do
      it "parses correct markup" $ do
        input <- readFile "markup/hours.html"
        let expected = Right $ CoursePage.Time 800 900
            actual   = CoursePage.parseTime input
        actual `shouldBe` expected

      it "fails on non parsable input" $ do
        let input    = "not hours"
            expected = parseError input "Hours"
            actual   = CoursePage.parseTime input :: Either AppError CoursePage.Time
        actual `shouldBe` expected
