module CourseScalpel.CoursePage.Header
  ( Header (..)
  , parse
  , scraper
  ) where

import           Data.Aeson                (ToJSON, FromJSON)
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc hiding (space)
import           Text.Megaparsec           hiding (parse)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec           as MP
import qualified Data.Text                 as T
import           Text.HTML.Scalpel
import GHC.Generics (Generic)

import           CourseScalpel.Error       (Error, parseError)
import           CourseScalpel.Parser      (Parser)
import qualified CourseScalpel.Parser      as Parser
import qualified CourseScalpel.Course      as Course
import           CourseScalpel.Examination (Credits (..))

data Header = Header
  { credits :: !Credits
  , code    :: !Text
  , name    :: !Text
  } deriving (Show, Eq, Generic)

instance FromJSON Header
instance ToJSON Header

{- A header can look like "Ingenjörsprofessionalism, del 1, 1 hp (TDDD70)"
   so by reversing and breaking on the comma (over here --^) the problem
   of parsing through the first comma is avoided. -}
parse :: Text -> Parser.Result Header
parse x = do
  let (credCodePart, namePart) = T.breakOn "," $ T.reverse x
  let eCredCode = MP.parse credCodeParser  ""  $ T.reverse credCodePart
  let eName     = MP.parse nameParser      ""  $ T.reverse $ T.tail namePart
  let eHeader   = uncurry Header <$> eCredCode <*> eName
  either hoistError pure eHeader

  where
    hoistError :: MP.ParseError Char Error -> Either Error Header
    hoistError = const $ parseError x "Course Page Header"

    credCodeParser :: Parser (Course.Credits, Text)
    credCodeParser = do
      _      <- space
      amount <- read <$> some digitChar
      _      <- string " hp "
      _      <- char '('
      code   <- T.pack <$> some alphaNumChar
      _      <- char ')'
      pure (Credits amount, code)

    nameParser :: Parser Text
    nameParser = T.pack <$> some (alphaNumChar <|> spaceChar <|> char ',')

instance Pretty Header where
  pretty Header{..} =
    pretty code <>
    ": "        <>
    pretty name <>
    ", "        <>
    pretty credits

scraper :: Scraper Text (Parser.Result Header)
scraper = chroot ("div" @: [hasClass "main-container"]) $ chroot "header" $
    parse <$> text "h1"
