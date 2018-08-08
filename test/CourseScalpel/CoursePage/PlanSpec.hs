module CourseScalpel.CoursePage.PlanSpec where

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Test.Hspec

import           CourseScalpel.Course          (Area (..), Credits (..),
                                                Field (..), Hours (..),
                                                Institution (..), Subject (..))
import qualified CourseScalpel.CoursePage.Plan as Plan
import           CourseScalpel.Examination     (Examination (..))
import qualified CourseScalpel.Examination     as Examination
import qualified CourseScalpel.Parser          as Parser
import           CourseScalpel.Web.Url         (Url (..))

spec :: SpecWith ()
spec = do
  describe "parseAreas" $ do
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
              [ AreaAppliedMaths
              , AreaComputerScience
              , AreaComputerEngineering
              , AreaElectrotechnic
              , AreaInformatics
              , AreaMaths
              , AreaMedicinalEngineering
              , AreaPhysics
              , AreaScience
              , AreaTechnical
              , AreaTechnicalPhysics
              , AreaOther
              ]
            actual = Plan.parseAreas input
        actual `shouldBe` expected

      it "fails to parse non-valid areas" $ do
        let input    = "not areas"
            actual   = Plan.parseAreas input
            expected = Parser.failure input "Areas"
        actual `shouldBe` expected

  describe "parseExaminations" $ do
    it "parses expected markup" $ do
      input <- T.readFile "test/markup/examination.html"
      let actual   = Plan.parseExaminations input
          expected = Right
              [ Examination "TEN1"
                Examination.TEN  "Skriftlig tentamen"
                Examination.Scale
                (Credits 4)
              , Examination "UPG1"
                Examination.UPG  "Uppgifter"
                Examination.Binary
                (Credits 28)
              , Examination "OPPO"
                Examination.OPPO "Opponering"
                Examination.Binary
                (Credits 2)
              , Examination "AUSK"
                Examination.AUSK "Auskonsultation"
                Examination.Presence
                (Credits 2)
              , Examination "PRA1"
                Examination.PROJ "Project"
                Examination.Presence
                (Credits 1.5)
              , Examination "DAT1"
                Examination.DAT  "Datortentamen"
                Examination.Binary
                (Credits 5)
              , Examination "HEM2"
                Examination.HEM  "Hemtentamen"
                Examination.Scale
                (Credits 6)
              ]

      actual`shouldBe` expected

    it "failes to parse invalid examinations" $ do
      let input    = "not examinations"
          actual   = Plan.parseExaminations input
          expected = Parser.failure input "Examinations"
      actual `shouldBe` expected

    it "parses expected markup" $ do
      input <- T.readFile "test/markup/examination-tams22.html"
      let expected1 = Examination
            { Examination.code        = "TEN1"
            , Examination.typ         = Examination.TEN
            , Examination.description = "En skriftlig tentamen (U,3,4,5)"
            , Examination.grading     = Examination.Scale
            , Examination.credits     = Credits 5
            }

          expected2 = Examination
            { Examination.code        = "LAB1"
            , Examination.typ         = Examination.LAB
            , Examination.description = "Obligatoriska inlämningsuppgifter (U,3,4,5)"
            , Examination.grading     = Examination.Scale
            , Examination.credits     = Examination.Credits 1
            }

          actual = Plan.parseExaminations input
      actual `shouldBe` Right [expected1, expected2]

    describe "parseUrls" $ do
      it "parses an url" $ do
        input <- T.readFile "test/markup/url.html"
        let expected = Right [Url "http://www.isy.liu.se/en/edu/kurs/TSTE12/"]
            actual   = Plan.parseUrls input
        actual `shouldBe` expected

      it "returns empty list on no urls" $ do
        let input    = ""
            expected = Right []
            actual   = Plan.parseUrls input
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
              [ FieldHumanities
              , FieldMedicine
              , FieldTechnical
              , FieldScience
              , FieldSociety
              ]

            actual = Plan.parseFields input
        actual `shouldBe` expected

      it "fails to parse illegal values" $ do
        let input    = "not a field"
            expected = sequenceA [Parser.failure input "Field"]
            actual   = Plan.parseFields input
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
              [ InstitutionMAI
              , InstitutionISY
              , InstitutionIDA
              , InstitutionIDA
              , InstitutionMED
              , InstitutionIFM
              , InstitutionIFM
              , InstitutionIEE
              , InstitutionTekFak
              ]

            actual = traverse Plan.parseInstitution input
        actual `shouldBe` expected

      it "parses white space padded values" $ do
        let input    = "Institutionen f\246r Fysik, kemi och biologi "
            expected = Right InstitutionIFM
            actual   = Plan.parseInstitution input
        actual `shouldBe` expected

      it "fails to parse non-valid values" $ do
        let input    = "not an institution"
            expected = Parser.failure input "Institution"
            actual   = Plan.parseInstitution input
        actual `shouldBe` expected

    describe "parseGrading" $ do
      it "parses valid values" $ do
        let input    = ["U, G", "U,G", "U, 3, 4, 5", "U,3,4,5", "Deltagit (D)", "D", ""]
            expected = Right
              [ Examination.Binary
              , Examination.Binary
              , Examination.Scale
              , Examination.Scale
              , Examination.Presence
              , Examination.Presence
              , Examination.Unspecified
              ]
            actual = traverse Plan.parseGrading input

        actual `shouldBe` expected

      it "fails to parse non-valid values" $ do
        let input    = "not a gradescale"
            expected = Parser.failure input "Grading"
            actual   = Plan.parseGrading input
        actual `shouldBe` expected

    describe "parseCredits" $ do
      it "parses integer" $
        Plan.parseCredits "5" `shouldBe` Right (Credits 5)

      it "parses integer with asterisk" $
        Plan.parseCredits "5*" `shouldBe` Right (Credits 5)

      it "parses integer will trailing hp" $
        Plan.parseCredits "5 hp" `shouldBe` Right (Credits 5)

      it "parses decimal with trailing hp" $
        Plan.parseCredits "1.5 hp" `shouldBe` Right (Credits 1.5)

      it "fails to parse non-numeric value" $ do
        let input = "not credit"
        Plan.parseCredits input `shouldBe` Parser.failure input "Credits"

    describe "parse CourseTime" $ do
      it "parses markup with padding" $ do
        input <- T.readFile "test/markup/hours-padding.html"
        let expected = Right (Hours 800, Hours 900)
            actual   = Plan.parseTime input
        actual `shouldBe` expected

      it "parses markup without padding" $ do
        input <- T.readFile "test/markup/hours.html"
        let expected = Right (Hours 800, Hours 900)
            actual   = Plan.parseTime input
        actual `shouldBe` expected

      -- This is a bug in some course markup, where
      -- it simply has a minus sign in front of the course time.
      it "parses incorrect markup with a minus sign" $ do
        let input = "Preliminary scheduled hours: 32 h <br>Recommended self-study hours: -5 h"
        let expected = Right (Hours 5, Hours 32)
            actual   = Plan.parseTime input
        actual `shouldBe` expected

      it "fails on non parsable input" $ do
        let input    = "not hours"
            expected = Parser.failure input "Hours"
            actual   = Plan.parseTime input
        actual `shouldBe` expected

    describe "parseSubjects" $
      it "parses subjects with a comma in the name" $ do
        let input    = "Ledarskap, organisation och styrning"
            expected = Right [SubjectLeadershipOrganisation]
            actual   = Plan.parseSubjects input
        actual `shouldBe` expected
