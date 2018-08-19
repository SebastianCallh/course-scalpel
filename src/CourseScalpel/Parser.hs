module CourseScalpel.Parser
  ( Parser
  , Result
  , parseError
  , takeMay
  , sanitize
  , nonEmpty
  ) where

import           Data.Text           (Text)
import qualified Data.Text           as T
import           Text.Megaparsec     (Parsec)
import qualified Text.Megaparsec     as MP

import           CourseScalpel.Error (Error, parseError)

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
    Left  _  -> parseError txt typ
    Right [] -> parseError txt typ
    Right a  -> pure a
