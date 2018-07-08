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

type HasCliError = MonadError CliError

data CliError
  = ParseProgramError Text
  | CourseScalpelError AppError

instance Pretty CliError where
  pretty (ParseProgramError txt) =
    pretty ("Error parsing program: " :: Text) <> pretty txt
  pretty (CourseScalpelError appError) =
    pretty ("Application error: " :: Text) <> pretty appError

readProgram :: HasCliError m => String -> m Program
readProgram "d"   = pure engD
readProgram "u"   = pure engU
readProgram "i"   = pure engI
readProgram "ii"  = pure engIInt
readProgram "it"  = pure engIT
readProgram "y"   = pure engY
readProgram "yi"  = pure engYInt
readProgram "med" = pure engMed
readProgram "mt"  = pure engMT
readProgram "ed"  = pure engED
readProgram "kts" = pure engKTS
readProgram "m"   = pure engM
readProgram "emm" = pure engEMM
readProgram "dpu" = pure engDPU
readProgram "tb"  = pure engTB
readProgram "kb"  = pure engKB
readProgram x     = throwError $ ParseProgramError $ "Could not read program: " <> T.pack x

writeProgram :: Program -> String
writeProgram (Program EngD  _)   = "d"
writeProgram (Program EngU _)    = "u"
writeProgram (Program EngIT _)   = "it"
writeProgram (Program EngI _)    = "i"
writeProgram (Program EngIInt _) = "ii"
writeProgram (Program EngY _)    = "y"
writeProgram (Program EngYInt _) = "yi"
writeProgram (Program EngMed _)  = "med"
writeProgram (Program EngMT _)   = "mt"
writeProgram (Program EngED _)   = "ed"
writeProgram (Program EngKTS _)  = "kts"
writeProgram (Program EngM _)    = "m"
writeProgram (Program EngEMM _)  = "emm"
writeProgram (Program EngTB _)   = "tb"
writeProgram (Program EngKB _)   = "kb"
writeProgram (Program EngDPU _)  = "dpu"
