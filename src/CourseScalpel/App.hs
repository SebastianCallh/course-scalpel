{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CourseScalpel.App
  ( App
  , runApp
  , Config (..)
  ) where

import           Control.Monad.IO.Class    (liftIO)

import           Control.Monad.Logger      (LoggingT, MonadLogger,
                                            runFileLoggingT)
import           Control.Monad.Reader      (MonadIO, MonadReader, ReaderT,
                                            runReaderT)
import           CourseScalpel.CoursePage  (MonadCoursePage (..))
import qualified CourseScalpel.CoursePage  as CoursePage
import           CourseScalpel.ProgramPage (MonadProgramPage (..))
import qualified CourseScalpel.ProgramPage as ProgramPage

data Config = Config
  { configLogFile :: FilePath
  }

--- App ---

newtype App a = App { unApp :: ReaderT Config (LoggingT IO) a }
  deriving (Functor, Applicative, Monad, MonadIO,
            MonadReader Config, MonadLogger)

instance MonadCoursePage App where
  scrapeCoursePage url = liftIO (CoursePage.scrape url)

instance MonadProgramPage App where
  scrapeProgramPage = ProgramPage.scrape

runApp :: Config -> App a -> IO a
runApp options = logRunner . readerRunner . unApp
  where
    logRunner      = runFileLoggingT $ configLogFile options
    readerRunner x = runReaderT x options
