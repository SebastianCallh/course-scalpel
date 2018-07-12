{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module CourseScalpel.Cli
  ( main
  ) where

import           Control.Monad             (forM, forM_)
import           Control.Monad.Except      (ExceptT (..), MonadError,
                                            runExceptT, throwError)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Logger      (LogLevel (..), logWithoutLoc)
import           Control.Monad.Logger      (LoggingT, MonadLogger,
                                            runFileLoggingT, runStdoutLoggingT)
import           Control.Monad.Reader      (asks)
import           Control.Monad.Reader      (MonadIO, MonadReader, ReaderT,
                                            runReaderT)
import           Control.Monad.Trans       (lift)
import           Data.Char                 (toLower)
import           Data.Either               (partitionEithers)
import           Data.List                 (intercalate)
import           Data.List.Split           (splitOn)
import           Data.Semigroup            ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import           Data.Text.Prettyprint.Doc
import           Data.Time
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Clock           (getCurrentTime)
import           Options.Applicative

import           CourseScalpel             (CourseScalpelRunner,
                                            ScrapeCourseRes (..),
                                            ScrapeProgramRes (..), Term (..),
                                            mkConfig, runCourseScalpel,
                                            scrapeCourse, scrapeProgram)
import           CourseScalpel.Error       (AppError)
import           CourseScalpel.Program     (Code (..), Program (..), engD,
                                            engDPU, engED, engEMM, engI,
                                            engIInt, engIT, engKB, engKTS, engM,
                                            engMT, engMed, engTB, engU, engY,
                                            engYInt, programCode,
                                            supportedPrograms)
import           CourseScalpel.Time        (Year (..))


type HasCliError = MonadError CliError

data CliError
  = ParseProgramError Text
  | CourseScalpelError AppError

instance Pretty CliError where
  pretty (ParseProgramError txt) =
    "Error parsing program: " <> pretty txt
  pretty (CourseScalpelError appError) =
    "Application error: " <> pretty appError

type CourseCodeStr  = String
type ProgramCodeStr = String

data Target
  = TargetPrograms [Program]
  | TargetCourse  CourseCodeStr

data Output
  = OutputFile FilePath
  | OutputStdOut

data Options = Options
  { optionsTarget  :: Target
  , optionsOutput  :: Output
  , optionsLogFile :: FilePath
  }

newtype CliApp a = CliApp { unCli :: ExceptT CliError (ReaderT Options (LoggingT IO)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError CliError,
            MonadReader Options, MonadLogger)

main :: IO ()
main = do
  (year, _, _) <- getCurrentTime >>= return . toGregorian . utctDay
  options      <- execParser . opts $ Year (fromIntegral year)
  result       <- runCliApp options cliApp
  case result of
    Left cliError -> putStrLn . show $ pretty cliError
    Right _       -> putStrLn "Done!"

runCliApp :: Options -> CliApp a -> IO (Either CliError a)
runCliApp options = logRunner . readerRunner . runExceptT . unCli
  where
    logRunner      = runStdoutLoggingT -- $ optionsLogFile options
    readerRunner x = runReaderT x options

cliApp :: CliApp ()
cliApp = do
  target  <- asks optionsTarget
  logFile <- asks optionsLogFile
  let runner = runCourseScalpel $ mkConfig logFile
  case target of
    TargetPrograms programs   -> scrapePrograms' runner programs
    TargetCourse   courseCode -> scrapeCourse'   runner courseCode

scrapePrograms'
  :: CourseScalpelRunner ScrapeProgramRes
  -> [Program]
  -> CliApp ()
scrapePrograms' runner programs = do
  logFilePath <- asks optionsLogFile
  putLn "Scraping programs..."
  results <- forM programs $ \program -> do
    putLn $ "Scraping program " <> (show $ programCode program)
    result <- liftIO . runner $ scrapeProgram program
    pure result

  outputResult results

scrapeCourse'
  :: CourseScalpelRunner ScrapeCourseRes
  -> CourseCodeStr
  -> CliApp ()
scrapeCourse' runner code = do
  putLn $ "Scraping course " <> code
  result <- liftIO . runner . scrapeCourse $ T.pack code
  outputResult [result]

outputResult :: Pretty a => [Either AppError a] -> CliApp ()
outputResult result =
  asks optionsOutput >>= \case
    OutputStdOut    -> do
      liftIO $ traverse (putStrLn . prettyEither) result
      pure ()

    OutputFile path -> do
      liftIO . writeFile path . show $ traverse prettyEither result
      putLn $ "Result written to " <> path

-- Would like to derive a (Pretty a, Pretty e) => Pretty (Either e a)
-- someplace instead of this, but it would be an orphan instance.
prettyEither :: Pretty a => Either AppError a -> String
prettyEither (Left error) = show $ pretty error
prettyEither (Right a)    = show $ pretty a

putLn :: String -> CliApp ()
putLn = liftIO . putStrLn

opts :: Year -> ParserInfo Options
opts year = info (optionsParser year <**> helper)
  ( fullDesc
  <> progDesc "Application for scraping course data from https://liu.se/studieinfo."
  <> header "Course Scalpel - a web scraper for Linköping University courses." )

optionsParser :: Year -> Parser Options
optionsParser year =
  Options         <$>
  targetParser    <*>
  outputParser    <*>
  logFileParser
--  yearParser year

targetParser :: Parser Target
targetParser =
  (TargetPrograms <$> programTarget) <|>
  (TargetCourse   <$> courseTarget)
  where
    programTarget = option (eitherReader programs)
      (  long "programs"
      <> short 'p'
      <> metavar "PROGRAMS"
      <> help "Target programs to scrape."
      )

    programs :: String -> Either String [Program]
    programs = traverse (program . fmap toLower) . splitOn " "
    program "d"    = pure engD
    program "u"    = pure engU
    program "i"    = pure engI
    program "iint" = pure engIInt
    program "it"   = pure engIT
    program "y"    = pure engY
    program "yint" = pure engYInt
    program "med"  = pure engMed
    program "ed"   = pure engED
    program "mt"   = pure engMT
    program "kts"  = pure engKTS
    program "m"    = pure engM
    program "emm"  = pure engEMM
    program "tb"   = pure engTB
    program "dpu"  = pure engDPU
    program "kb"   = pure engKB
    program x      = Left $ "Can not parse Program from: " <> x

    courseTarget = strOption
      (  long "course"
         <> short 'c'
         <> metavar "COURSE_CODE"
         <> help (mconcat
         [ "Target course to scrape.\n"
         , "Example: \"-p d\", \"-p 'd it u'\"\n"
         , "Available programs: "
         , intercalate " " $ writeProgram <$> supportedPrograms
         ])
      )

outputParser :: Parser Output
outputParser = fileOutput <|> stdOutOutput
  where
    stdOutOutput = pure OutputStdOut
    fileOutput =
      OutputFile <$> strOption
      (  long "output"
        <> short 'o'
        <> metavar "FILE"
        <> help (mconcat
        [ "Specify a file to save output to. \n"
        , "Example \"-p courses.txt\""
        ])
      )

logFileParser :: Parser FilePath
logFileParser = customLogFile <|> defaultLogFile
  where
    defaultLogFile = pure "log.txt"
    customLogFile  = strOption
      (  long "log"
        <> short 'l'
        <> metavar "FILE"
        <> help (mconcat
        [ "Specify a file to save log output to. \n"
        , "Example \"-l log.txt\""
        ])
      )

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
