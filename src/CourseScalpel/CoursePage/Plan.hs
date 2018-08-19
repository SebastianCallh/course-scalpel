module CourseScalpel.CoursePage.Plan
  ( Plan (..)
  , parseAreas
  , parseCredits
  , parseExaminations
  , parseFields
  , parseGrading
  , parseInstitution
  , parseSubjects
  , parseTime
  , parseUrls
  , scraper
  ) where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Map                  (Map)
import qualified Data.Map                  as M
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc (pretty)
import           Data.Text.Prettyprint.Doc hiding (space)
import           GHC.Generics              (Generic)
import           Text.HTML.Scalpel
import           Text.Megaparsec           hiding (parse)
import qualified Text.Megaparsec           as MP
import           Text.Megaparsec.Char

import qualified CourseScalpel.Course      as Course
import           CourseScalpel.Examination (Examination (..))
import qualified CourseScalpel.Examination as Examination
import           CourseScalpel.Parser      (Parser, parseError)
import qualified CourseScalpel.Parser      as Parser
import           CourseScalpel.Time        (Hours (..))
import           CourseScalpel.Web.Url     (Url (..))

data Plan = Plan
  { areas         :: ![Course.Area]
  , institution   :: !Course.Institution
  , fields        :: ![Course.Field]
  , level         :: !Course.Level
  , prerequisites :: !(Maybe Course.Prerequisites)
  , grades        :: !Examination.Grading
  , examinator    :: !(Maybe Course.Examinator)
  , examinations  :: ![Examination]
  , content       :: !Course.Content
  , subjects      :: ![Course.Subject]
  , urls          :: ![Url]
  , selfStudyTime :: !Hours
  , scheduledTime :: !Hours
  } deriving (Show, Eq, Generic)

instance FromJSON Plan
instance ToJSON Plan

instance Pretty Plan where
  pretty _ = "Not yet implemented."

scraper :: Scraper Text (Parser.Result Plan)
scraper = do
  sections <- chroot ("section" @: [hasClass "studyguide-block"]) $
    chroots "div" $ do
      title   <- innerHTML "h3"
      content <- Parser.sanitize . snd . T.breakOnEnd "</h3>" <$> innerHTML "div"
      return (title, content)
  pure $ fromPageSections . M.fromList $ sections

  where
    fromPageSections :: Map Text Text -> Parser.Result Plan
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
      <*> (fst <$> times)
      <*> (snd <$> times)
      where
        times = parseSection parseTime          Nothing              "Undervisningstid"
        parseSection :: (Text -> Parser.Result a) -> Maybe a -> Text -> Parser.Result a
        parseSection parse def key =
          case M.lookup key sections of
            Nothing      -> maybe (makeError key) pure  def
            Just section -> parse section

        makeError :: Text -> Parser.Result a
        makeError secKey = parseError
          ("Empty section " <> secKey) "CoursePagePlan"

--- Area ---

parseAreas :: Text -> Parser.Result [Course.Area]
parseAreas txt = Parser.nonEmpty txt "Areas" areas
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

--- Institution ---

parseInstitution :: Text -> Parser.Result Course.Institution
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

parseFields :: Text -> Parser.Result [Course.Field]
parseFields = traverse (parseField . T.strip) . T.splitOn ","

parseField :: Text -> Parser.Result Course.Field
parseField "Humanistiska omr\229det"             = pure Course.FieldHumanities
parseField "Medicinska omr\229det"               = pure Course.FieldMedicine
parseField "Tekniska omr\229det"                 = pure Course.FieldTechnical
parseField "Naturvetenskapliga omr\229det"       = pure Course.FieldScience
parseField "Samh\228llsvetenskapliga omr\229det" = pure Course.FieldSociety
parseField "Juridiska området"                   = pure Course.FieldLaw
parseField x                                     = parseError x "Field"

--- Prerequisites ---

parsePrerequisites :: Text -> Parser.Result (Maybe Course.Prerequisites)
parsePrerequisites = pure . pure . Course.Prerequisites

--- Subject ---
parseSubjects :: Text -> Parser.Result [Course.Subject]
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

-- | Slightly different from the other parsers as it
--   parses both self study and scheduled time. They are in the
--   same section in the DOM so this is the most convenient way.
parseTime :: Text -> Parser.Result (Hours, Hours)
parseTime x =
  either (const $ parseError x "Hours") pure $
  MP.parse parser "" (T.strip x)
    where
      parser :: Parser (Hours, Hours)
      parser = do
        scheduled <- fmap read $ prelTxt
          *> optional (char '-')
          *> some digitChar
          <* string " h <br>"
        selfStudy <- fmap read $ recTxt
          *> optional (char '-')
          *> some digitChar
          <* string " h"
        pure (Hours selfStudy, Hours scheduled)

      prelTxt = string "Preliminary scheduled hours: " <|>
                string "Preliminär schemalagd tid: "
      recTxt  = string "Recommended self-study hours: " <|>
                string "Rekommenderad självstudietid: "

--- Content ---

parseContent :: Text -> Parser.Result Course.Content
parseContent = pure . Course.Content

--- Examinator ---

parseExaminator :: Text -> Parser.Result (Maybe Course.Examinator)
parseExaminator = pure . pure . Course.Examinator

--- Examination ---

parseExaminations :: Text -> Parser.Result [Examination]
parseExaminations x =
    let mFieldList = scrapeStringLike x $
          chroot "table" $ innerHTMLs "tr"
    in case mFieldList of
      Nothing        -> parseError x "Examinations"
      Just fieldList -> traverse parseExamination fieldList

parseExamination :: Text -> Parser.Result Examination
parseExamination x =
    let mFields = scrapeStringLike x $ texts "td"
    in case mFields >>= Parser.takeMay 4 of
      Nothing -> parseError x "Examination"
      Just [code, description, grading, credits] ->
        Examination code
          <$> parseExaminationType code
          <*> pure description
          <*> parseGrading grading
          <*> parseCredits credits
      Just _ ->  parseError x "Examination"

--- ExaminationType ---

parseExaminationType :: Text -> Parser.Result Examination.Type
parseExaminationType x
  | "ANN"  `T.isPrefixOf` x = pure Examination.ANN
  | "AUSK" `T.isPrefixOf` x = pure Examination.AUSK
  | "BAS"  `T.isPrefixOf` x = pure Examination.BAS
  | "DAT"  `T.isPrefixOf` x = pure Examination.DAT
  | "HEM"  `T.isPrefixOf` x = pure Examination.HEM
  | "KTR"  `T.isPrefixOf` x = pure Examination.KTR
  | "LAB"  `T.isPrefixOf` x = pure Examination.LAB
  | "MOM"  `T.isPrefixOf` x = pure Examination.MOM
  | "MUN"  `T.isPrefixOf` x = pure Examination.MUN
  | "OPPO" `T.isPrefixOf` x = pure Examination.OPPO
  | "PRA"  `T.isPrefixOf` x = pure Examination.PROJ
  | "TEN"  `T.isPrefixOf` x = pure Examination.TEN
  | "UPG"  `T.isPrefixOf` x = pure Examination.UPG
  | otherwise               = parseError x "ExaminationType"

--- Grading ---

parseGrading :: Text -> Parser.Result Examination.Grading
parseGrading "U, 3, 4, 5"   = pure Examination.Scale
parseGrading "U,3,4,5"      = pure Examination.Scale
parseGrading "U, G"         = pure Examination.Binary
parseGrading "U,G"          = pure Examination.Binary
parseGrading "Deltagit (D)" = pure Examination.Presence
parseGrading "D"            = pure Examination.Presence
parseGrading ""             = pure Examination.Unspecified
parseGrading x              = parseError x "Grading"

--- Credits ---

parseCredits :: Text -> Parser.Result Course.Credits
parseCredits x =
  either errorOut mkCredit $ MP.parse parser "" $ T.strip x
  where
    errorOut = const $ parseError x "Credits"
    mkCredit = pure . Course.Credits . read
    parser :: Parser String
    parser   = some floatChar <* optional (space *> string "hp")
    floatChar = digitChar <|> char '.'

--- Level ---

parseLevel :: Text -> Parser.Result Course.Level
parseLevel "G1X" = pure Course.LevelG1
parseLevel "G2X" = pure Course.LevelG2
parseLevel "A"   = pure Course.LevelA
parseLevel "A1X" = pure Course.LevelA1
parseLevel "A2X" = pure Course.LevelA2
parseLevel x     = parseError x "Level"

--- Url ---

parseUrls :: Text -> Parser.Result [Url]
parseUrls x = do
  let mThing = scrapeStringLike x $ attrs "href" "a"
  let thing  = maybe [] (fmap Url) mThing :: [Url]
  pure thing
