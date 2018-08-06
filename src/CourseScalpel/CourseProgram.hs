{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module CourseScalpel.CourseProgram
  ( CourseProgram (..)
  , Semester (..)
  , Period (..)
  , Block (..)
  , Importance (..)
  ) where

import           Data.Aeson                    (FromJSON, ToJSON)
import           Data.Data                     (Typeable)
import           GHC.Generics                  (Generic)
import           Test.QuickCheck               (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary,
                                                genericArbitrary)

import           CourseScalpel.Program         (Program)

--- Program ---

data CourseProgram = CourseProgram
  { program    :: !Program
  , semester   :: !Semester
  , periods    :: ![Period]
  , blocks     :: ![[Block]]
  , importance :: !Importance
  } deriving (Show, Read, Eq, Ord,
              Typeable, Generic, FromJSON, ToJSON)

--- Importance ---

data Importance = V | O | F | OV
  deriving (Show, Read, Eq, Ord,
            Typeable, Generic, FromJSON, ToJSON)

--- Semester ---

data Semester
  = SemesterOne
  | SemesterTwo
  | SemesterThree
  | SemesterFour
  | SemesterFive
  | SemesterSix
  | SemesterSeven
  | SemesterEight
  | SemesterNine
  | SemesterTen
  deriving (Show, Read, Ord, Eq,
            Typeable, Generic, ToADTArbitrary,
            FromJSON, ToJSON)

instance Arbitrary Semester where
  arbitrary = genericArbitrary

--- Period ---

data Period = PeriodOne | PeriodTwo
  deriving (Show, Read, Eq, Typeable,
            Generic, ToADTArbitrary, FromJSON, ToJSON)

instance Ord Period where
  compare PeriodOne PeriodTwo = LT
  compare PeriodOne PeriodOne = EQ
  compare PeriodTwo PeriodOne = GT
  compare PeriodTwo PeriodTwo = EQ

instance Arbitrary Period where
  arbitrary = genericArbitrary

--- Block ---

data Block
  = BlockNil
  | BlockOne
  | BlockTwo
  | BlockThree
  | BlockFour
  | BlockNone
  deriving (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)

instance Ord Block where
  compare BlockNil BlockNone    = GT
  compare BlockNil BlockNil     = EQ
  compare BlockNil BlockOne     = LT
  compare BlockNil BlockTwo     = LT
  compare BlockNil BlockThree   = LT
  compare BlockNil BlockFour    = LT
  compare BlockOne BlockNone    = GT
  compare BlockOne BlockNil     = GT
  compare BlockOne BlockOne     = EQ
  compare BlockOne BlockTwo     = LT
  compare BlockOne BlockThree   = LT
  compare BlockOne BlockFour    = LT
  compare BlockTwo BlockNone    = GT
  compare BlockTwo BlockNil     = GT
  compare BlockTwo BlockOne     = GT
  compare BlockTwo BlockTwo     = EQ
  compare BlockTwo BlockThree   = LT
  compare BlockTwo BlockFour    = LT
  compare BlockThree BlockNone  = GT
  compare BlockThree BlockNil   = GT
  compare BlockThree BlockOne   = GT
  compare BlockThree BlockTwo   = GT
  compare BlockThree BlockThree = EQ
  compare BlockThree BlockFour  = LT
  compare BlockFour BlockNone   = GT
  compare BlockFour BlockNil    = GT
  compare BlockFour BlockOne    = GT
  compare BlockFour BlockTwo    = GT
  compare BlockFour BlockThree  = GT
  compare BlockFour BlockFour   = EQ
  compare BlockNone BlockNone   = EQ
  compare BlockNone BlockNil    = LT
  compare BlockNone BlockOne    = GT
  compare BlockNone BlockTwo    = GT
  compare BlockNone BlockThree  = GT
  compare BlockNone BlockFour   = GT
