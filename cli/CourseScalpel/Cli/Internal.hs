{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module CourseScalpel.Cli.Internal where

import           Control.Monad.Except      (MonadError, throwError)
import           Data.Semigroup            ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc

import           CourseScalpel.Error       (AppError (..))
import           CourseScalpel.Program     (Code (..), Program (..), engD,
                                            engDPU, engED, engEMM, engI,
                                            engIInt, engIT, engKB, engKTS, engM,
                                            engMT, engMed, engTB, engU, engY,
                                            engYInt, supportedPrograms)
--import qualified CourseScalpel.Program as Program
