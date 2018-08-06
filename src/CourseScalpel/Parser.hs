module CourseScalpel.Parser
  ( Parser
  , Result
  , Error (..)
  , failure
  , takeMay
  , sanitize
  , nonEmpty
  ) where

import           Data.Semigroup        ((<>))
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Text.Megaparsec       (Parsec)
import           Text.Megaparsec       hiding (failure, parse)
import qualified Text.Megaparsec       as MP

import           CourseScalpel.Web.Url (Url (..))

data Error
  = ParseError Text Text
  | NetworkError Url
  | UnsupportedProgram Text
  deriving (Eq, Ord)

instance Show Error where
  show = showErrorComponent

instance ShowErrorComponent Error where
  showErrorComponent (ParseError txt typ) =
    T.unpack $ "Could not parse '" <> txt <> "' as " <> typ <> "."
  showErrorComponent (UnsupportedProgram slug) =
    T.unpack $ "No supported program for slug " <> slug <> "."
  showErrorComponent (NetworkError url) =
    T.unpack $ "Could not connect to " <> getUrl url <> "."

type Parser = Parsec Error Text

type Result = Either Error

takeMay :: Int -> [a] -> Maybe [a]
takeMay n as
  | n > 0 && null as = Nothing
  | n == 0           = pure []
  | otherwise        =
    (head as :) <$> takeMay (pred n) (tail as)

-- | Strips leading and trailing whitespace and removes
--   junk characters, typically from text inside tags.
sanitize :: Text -> Text
sanitize = T.strip . T.filter (not . isTrash)
  where isTrash = (`elem` ['\t', '\n', '\r'])

nonEmpty :: Text -> Text -> Parser [a] -> Either Error [a]
nonEmpty txt typ parser =
  case MP.parse parser "" txt of
    Left  _  -> failure txt typ
    Right [] -> failure txt typ
    Right a  -> pure a

failure :: Text -> Text -> Either Error a
failure txt typ = Left $ ParseError txt typ
