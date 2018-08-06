{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module CourseScalpel.Program
  ( Program (..)
  , Slug (..)
  , Code (..)
  , parseSlug
  , fromSlug
  , slugToText
  , engD
  , engU
  , engI
  , engIInt
  , engIT
  , engY
  , engYInt
  , engMed
  , engMT
  , engED
  , engKTS
  , engM
  , engEMM
  , engTB
  , engDPU
  , engKB
  , supportedPrograms
  ) where

import           Data.Aeson                    (FromJSON, ToJSON)
import           Data.Data                     (Typeable)
import           Data.Semigroup                ((<>))
import           Data.Text                     (Text)
import           GHC.Generics                  (Generic)
import           Test.QuickCheck               (Arbitrary, arbitrary, oneof)
import           Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary,
                                                genericArbitrary)

--- Program ---

data Program = Program
  { code :: !Code
  , slug :: !Slug
  } deriving (Show, Read, Eq, Ord, Typeable, Generic, FromJSON, ToJSON)

instance Arbitrary Program where
  arbitrary = oneof $ pure <$> supportedPrograms

--- Code ---

data Code
  = EngD         -- Datateknik
  | EngU         -- Mjukvaruteknik
  | EngI         -- Industriell ekonomi
  | EngIInt      -- Industriell ekonomi - Internationell
  | EngIT        -- Informationsteknologi
  | EngY         -- Teknisk fysik och elektroteknik
  | EngYInt      -- Teknisk fysik och elektroteknik - Internationell
  | EngMed       -- Medicinsk teknik
  | EngED        -- Elektronikdesign
  | EngMT        -- Mediateknik
  | EngKTS       -- Kommunikation, transport och samhälle
  | EngM         -- Maskinteknik
  | EngEMM       -- Energi, miljö och management
  | EngTB        -- Teknisk biologi
  | EngDPU       -- Design och produktutveckling
  | EngKB        -- Kemisk biologi
  deriving (Show, Read, Eq, Ord, Typeable, Generic, ToADTArbitrary, FromJSON, ToJSON)

instance Arbitrary Code where
  arbitrary = genericArbitrary

--- Slug ---

--- These codes are prefixed with a P
-- compared to the ones used on studieinfo.
data Slug
  -- Engineering
  = P6CDDD -- D
  | P6CMJU -- U
  | P6CIII -- I
  | P6CIEI -- IInt
  | P6CITE -- IT
  | P6CYYY -- Y
  | P6CYYI -- YInt
  | P6CMED -- Med
  | P6CMEN -- MT
  | P6CIEN -- ED
  | P6CKTS -- KTS
  | P6CMMM -- M
  | P6CEMM -- EMM
  | P6CDPU -- DPU
  | P6CTBI -- TB
  | P6CKEB -- KB
  deriving (Show, Read, Eq, Ord, Typeable, Generic, ToADTArbitrary, FromJSON, ToJSON)

instance Arbitrary Slug where
  arbitrary = genericArbitrary

parseSlug :: Text -> Either Text Slug
parseSlug "6CDDD" = pure P6CDDD -- D
parseSlug "6CMJU" = pure P6CMJU -- U
parseSlug "6CIII" = pure P6CIII -- I
parseSlug "6CIEI" = pure P6CIEI -- IInt
parseSlug "6CITE" = pure P6CITE -- IT
parseSlug "6CYYY" = pure P6CYYY -- Y
parseSlug "6CYYI" = pure P6CYYI -- YInt
parseSlug "6CMED" = pure P6CMED -- Med
parseSlug "6CMEN" = pure P6CMEN -- MT
parseSlug "6CIEN" = pure P6CIEN -- ED
parseSlug "6CKTS" = pure P6CKTS -- KTS
parseSlug "6CMMM" = pure P6CMMM -- M
parseSlug "6CEMM" = pure P6CEMM -- EMM
parseSlug "6CDPU" = pure P6CDPU -- DPU
parseSlug "6CTBI" = pure P6CTBI -- TB
parseSlug "6CKEB" = pure P6CKEB -- KB
parseSlug x       =
  Left $ "Could not parse "
  <> x <> " as ProgramSlug."

slugToText :: Slug -> Text
slugToText P6CDDD = "6CDDD"
slugToText P6CMJU = "6CMJU"
slugToText P6CIII = "6CIII"
slugToText P6CIEI = "6CIEI"
slugToText P6CITE = "6CITE"
slugToText P6CYYY = "6CYYY"
slugToText P6CYYI = "6CYYI"
slugToText P6CMED = "6CMED"
slugToText P6CMEN = "6CMEN"
slugToText P6CIEN = "6CIEN"
slugToText P6CKTS = "6CKTS"
slugToText P6CMMM = "6CMMM"
slugToText P6CEMM = "6CEMM"
slugToText P6CDPU = "6CDPU"
slugToText P6CTBI = "6CTBI"
slugToText P6CKEB = "6CKEB"

fromSlug :: Slug -> Program
fromSlug P6CDDD = engD    -- D
fromSlug P6CMJU = engU    -- U
fromSlug P6CIII = engI    -- I
fromSlug P6CIEI = engIInt -- IInt
fromSlug P6CITE = engIT   -- IT
fromSlug P6CYYY = engY    -- Y
fromSlug P6CYYI = engYInt -- YInt
fromSlug P6CMED = engMed  -- Med
fromSlug P6CMEN = engMT   -- MT
fromSlug P6CIEN = engED   -- ED
fromSlug P6CKTS = engKTS  -- KTS
fromSlug P6CMMM = engM    -- M
fromSlug P6CEMM = engEMM  -- EMM
fromSlug P6CDPU = engDPU  -- DPU
fromSlug P6CTBI = engTB   -- TB
fromSlug P6CKEB = engKB   -- KB

-- Engineering
engD :: Program
engD = Program EngD  P6CDDD

engU :: Program
engU = Program EngU P6CMJU

engI :: Program
engI  = Program EngI P6CIII

engIInt :: Program
engIInt = Program EngIInt P6CIEI

engIT :: Program
engIT = Program EngIT P6CITE

engY  :: Program
engY  = Program EngY P6CYYY

engYInt :: Program
engYInt = Program EngYInt P6CYYI

engMed :: Program
engMed = Program EngMed P6CMED

engMT :: Program
engMT = Program EngMT P6CMEN

engED :: Program
engED = Program EngED P6CIEN

engKTS :: Program
engKTS = Program EngKTS P6CKTS

engM :: Program
engM = Program EngM P6CMMM

engEMM :: Program
engEMM = Program EngEMM P6CEMM

engTB :: Program
engTB = Program EngTB P6CTBI

engDPU :: Program
engDPU = Program EngDPU P6CDPU

engKB :: Program
engKB = Program EngKB P6CKEB

supportedPrograms :: [Program]
supportedPrograms =
  [ engD
  , engU
  , engI
  , engIInt
  , engIT
  , engY
  , engYInt
  , engMed
  , engMT
  , engED
  , engKTS
  , engM
  , engEMM
  , engTB
  , engDPU
  , engKB
  ]
