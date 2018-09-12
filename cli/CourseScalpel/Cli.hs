{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module CourseScalpel.Cli
  ( main
  ) where

import           Control.Monad             (forM)
import           Control.Monad.Except      (ExceptT (..), MonadError,
                                            runExceptT)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Logger      (LoggingT, MonadLogger,
                                            runStdoutLoggingT)
import           Control.Monad.Reader      (MonadIO, MonadReader, ReaderT, asks,
                                            runReaderT)
import           Data.Aeson                (ToJSON)
import           Data.Aeson.Text           (encodeToLazyText)
import           Data.Char                 (toLower)
import           Data.List.Split           (splitOn)
import           Data.Semigroup            ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as L
import           Data.Text.Prettyprint.Doc (Pretty, pretty)
import           Options.Applicative

import           CourseScalpel             (CourseScalpelRunner,
                                            ProgramPageScrapeResult,
                                            ScrapeCoursePageResult, mkConfig,
                                            runCourseScalpel, scrapeCoursePage,
                                            scrapeProgramPage)
import           CourseScalpel.Error       (Error)

import           CourseScalpel.Program     (Program (..))
import qualified CourseScalpel.Program     as Program

data CliError
  = ParseProgramError Text
  | CourseScalpelError Error

instance Pretty CliError where
  pretty (ParseProgramError txt) =
    "Error parsing program: " <> pretty txt
  pretty (CourseScalpelError appError) =
    "Application error: " <> pretty appError

type CourseCodeStr  = String

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
  options <- execParser opts
  result  <- runCliApp options cliApp
  case result of
    Left cliError -> print $ pretty cliError
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
  :: CourseScalpelRunner ProgramPageScrapeResult
  -> [Program]
  -> CliApp ()
scrapePrograms' runner programs = do
  _logFilePath <- asks optionsLogFile
  putLn "Scraping programs..."
  results <- forM programs $ \program -> do
    putLn $ "Scraping program " <> show (Program.code program)
    liftIO . runner $ scrapeProgramPage program

  outputResult results

scrapeCourse'
  :: CourseScalpelRunner ScrapeCoursePageResult
  -> CourseCodeStr
  -> CliApp ()
scrapeCourse' runner code' = do
  putLn $ "Scraping course " <> code'
  eCoursePage <- liftIO . runner $ scrapeCoursePage $ T.pack code'
  outputResult [eCoursePage]

outputResult :: ToJSON a => [Either Error a] -> CliApp ()
outputResult results =
  asks optionsOutput >>= \case
    OutputStdOut -> do
      _ <- liftIO  $ traverse (putStrLn . toStr) results
      pure ()

    OutputFile path ->
      case sequence results of
        Left  err -> do
          liftIO . print $ pretty err
          putLn "There were errors. No results written."

        Right scrapeRes -> do
          liftIO . writeFile path $ toStr scrapeRes
          putLn $ "Results written to " <> path <> " ."

  where
    toStr :: ToJSON a => a -> String
    toStr = T.unpack . L.toStrict  . encodeToLazyText

putLn :: String -> CliApp ()
putLn = liftIO . putStrLn

opts :: ParserInfo Options
opts = info (optionsParser <**> helper)
  ( fullDesc
  <> progDesc "Application for scraping course data from https://liu.se/studieinfo."
  <> header "Course Scalpel - a web scraper for Linköping University courses." )

optionsParser :: Parser Options
optionsParser =
  Options         <$>
  targetParser    <*>
  outputParser    <*>
  logFileParser

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
    program "d"    = pure Program.engD
    program "u"    = pure Program.engU
    program "i"    = pure Program.engI
    program "iint" = pure Program.engIInt
    program "it"   = pure Program.engIT
    program "y"    = pure Program.engY
    program "yint" = pure Program.engYInt
    program "med"  = pure Program.engMed
    program "ed"   = pure Program.engED
    program "mt"   = pure Program.engMT
    program "kts"  = pure Program.engKTS
    program "m"    = pure Program.engM
    program "emm"  = pure Program.engEMM
    program "tb"   = pure Program.engTB
    program "dpu"  = pure Program.engDPU
    program "kb"   = pure Program.engKB
    program x      = Left $ "Can not parse Program from: " <> x

    courseTarget = strOption
      (  long "course"
         <> short 'c'
         <> metavar "COURSE_CODE"
         <> help (mconcat
         [ "Target course to scrape.\n"
         , "Example: \"-p d\", \"-p 'd it u'\"\n"
         , "Available programs: "
         , unwords
           $ T.unpack
           . Program.codeToText
           . Program.code
           <$> Program.supportedPrograms
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
