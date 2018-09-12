{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module CourseScalpel.Error
  ( Error (..)
  , Type
  , MonadError
  , parseError
  , unsupportedProgramError
  , filterUnsupportedProgramErrors
  ) where

import qualified Control.Monad.Except      as Except
import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Semigroup            (Semigroup (..), (<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc
import           GHC.Generics              (Generic)
import           Prelude                   hiding (error)

type Slug    = Text
type Type    = Text
data Error
  = ParseError   Text Type
  | UnsupportedProgramError Slug
  deriving (Eq, Ord, Generic)

instance ToJSON Error
instance FromJSON Error

instance Show Error where
  show = T.unpack . toText

instance Pretty Error where
  pretty = pretty . toText

parseError :: MonadError m => Text -> Type -> m a
parseError txt typ = error $ ParseError txt typ

unsupportedProgramError :: MonadError m => Slug -> m a
unsupportedProgramError = error . UnsupportedProgramError

toText :: Error -> Text
toText (ParseError txt typ)
  =  "Could not parse '"
  <> txt
  <> "' as "
  <> typ
  <> "."

toText (UnsupportedProgramError slug)
  =  "No supported program for slug "
  <> slug
  <> "."

-- | May want to change error handling, so it gets its own type
type MonadError = Except.MonadError Error

-- | May want to change error handling so wrap throwError
error :: MonadError m => Error -> m a
error = Except.throwError

filterUnsupportedProgramErrors :: [Either Error a] -> [Either Error a]
filterUnsupportedProgramErrors = filter supportedProgram
  where
    supportedProgram (Left (UnsupportedProgramError _)) = False
    supportedProgram _                                  = True
