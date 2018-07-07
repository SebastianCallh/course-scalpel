{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module CourseScalpel.Program
  ( Program (..)
  , Slug (..)
  , Code (..)
  , supportedPrograms
  , parseSlug
  , fromSlug
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
  ) where

import           Test.QuickCheck               (Arbitrary, arbitrary, oneof)
import           Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary,
                                                genericArbitrary)

import           Data.Aeson                    (FromJSON, ToJSON)
import           Data.Data                     (Typeable)
import           Data.Text                     (Text)
import           GHC.Generics                  (Generic)

import           CourseScalpel.Error           (HasError)
import           CourseScalpel.Parsing         (parseError)

--- Program ---

data Program = Program
  { programCode :: !Code
  , programSlug :: !Slug
  } deriving (Show, Read, Eq, Typeable, Generic, FromJSON, ToJSON)

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
  deriving (Show, Read, Eq, Typeable, Generic, ToADTArbitrary, FromJSON, ToJSON)

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
  deriving (Show, Read, Eq, Typeable, Generic, ToADTArbitrary, FromJSON, ToJSON)

instance Arbitrary Slug where
  arbitrary = genericArbitrary

parseSlug :: HasError m => Text -> m Slug
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
parseSlug x       = parseError x "ProgramSlug"

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

{- It was a paint to extract these so they can stay should they be needed
data Program = Program
  { programCode :: !ProgramCode
  , programSlug :: !ProgramSlug
  } deriving (Show, Read, Eq)

instance Arbitrary Program where
  arbitrary = Program
    <$> arbitrary
    <*> arbitrary

sep :: T.Text
sep = ":"

bachD = Program BachD       P6IDAT
bachElec =  Program BachElec    P6IELK
bachConst = Program BachConst   P6IBYG
bachKA =  Program BachKA      P6IKEA
bachM = Program BachM       P6IMAS
bachBio  = Program BachBio     P6KBIO
bachExp = Program BachExp     PMGMB2
bachCult = Program BachCult    PF7KKU
bachSoc = Program BachSoc     PF7KSP
bachSys = Program BachSys     PF7KSY
bachChem = Program BachChem    P6KKEB
bachCog = Program BachCog     PF7KKO
bachKSM = Program BachKSM     PF7KKM
bachIP  = Program BachIP      P6KIPR
bachAer = Program BachAer     P6KFTL
bachLog = Program BachLog     P6KLOG
bachMol = Program BachMol     P6KKEM
bachNano = Program BachNano    P6KFYN
bachMath  = Program BachMath    P6KMAT
bachGDK  = Program BachGDK     P6KGDK
bachSC  = Program BachSC      PF7KSK
bachPol = Program BachPol     PF7KPO
bachStat  = Program BachStat    PF7KSA
bachEnv  = Program BachEnv     PF7KMO
bachLaw  = Program BachLaw     PF7KAA

  -- Engineering
engD  = Program EngD        P6CDDD
engU  = Program EngU        P6CMJU
engI  = Program EngI        P6CIII
engIInt = Program EngIInt     P6CIEI
engIT   = Program EngIT       P6CITE
engY    = Program EngY        P6CYYY
engYInt = Program EngYInt     P6CYYI
engMed  = Program EngMed      P6CMED
engMT   = Program EngMT       P6CMEN
engED   = Program EngED       P6CIEN
engKTS  = Program EngKTS      P6CKTS
engM    = Program EngM        P6CMMM
engEMM  = Program EngEMM      P6CEMM
engTB   = Program EngTB       P6CTBI
engDPU  = Program EngDPU      P6CDPU
engKB   = Program EngKB       P6CKEB

  -- Master programs
masterCS = Program MasterCS    P6MICS
masterDAV = Program MasterDAV   P6MDAV
masterStat = Program MasterStat PF7MSL
masterSoc = Program  MasterSoc   PF7MSA
masterPS  = Program MasterPS    P6MPRO
masterITSL = Program MasterITSL  P6MTSL
masterM   = Program MasterM     P6MMEC
masterHR  = Program MasterHR    PF7MHR
masterI  = Program MasterI     PF7MHR
masterEth = Program MasterEth   PF7MEM
masterCom  = Program MasterCom   P6MCSY
masterAer = Program MasterAer   P6MAER
masterEco  = Program MasterEco   P6MECO
masterNano  = Program MasterNano  P6MMSN
masterElec = Program MasterElec  P6MELE
masterPhys  = Program MasterPhys  P6MFYS
masterAnim  = Program MasterAnim  P6METH
masterLearn = Program MasterLearn PL7MLG
masterCog  = Program MasterCog   PF7MKS
masterMed  = Program MasterMed   PMMIN1
masterGen  = Program MasterGen   PF7MGE
masterDes  = Program MasterDes   P6MDES
masterMath = Program MasterMath  P6MMAT
masterSus  = Program MasterSus   P6MSUS
masterBio  = Program MasterBio   P6MBME
masterEnv  = Program MasterEnv   PL7MOS

allPrograms :: [Program]
allPrograms =
  masterPrograms <>
  bachPrograms   <>
  engPrograms

engPrograms :: [Program]
engPrograms =
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

bachPrograms :: [Program]
bachPrograms =
    [ bachD
  , bachElec
  , bachConst
  , bachKA
  , bachM
  , bachBio
  , bachExp
  , bachCult
  , bachSoc
  , bachSys
  , bachChem
  , bachCog
  , bachKSM
  , bachIP
  , bachAer
  , bachLog
  , bachMol
  , bachNano
  , bachMath
  , bachGDK
  , bachSC
  , bachPol
  , bachStat
  , bachEnv
  , bachLaw
  ]

masterPrograms :: [Program]
masterPrograms =
  [ masterCS
  , masterDAV
  , masterStat
  , masterSoc
  , masterPS
  , masterITSL
  , masterM
  , masterHR
  , masterI
  , masterEth
  , masterCom
  , masterAer
  , masterEco
  , masterNano
  , masterElec
  , masterPhys
  , masterAnim
  , masterLearn
  , masterCog
  , masterMed
  , masterGen
  , masterDes
  , masterMath
  , masterSus
  , masterBio
  , masterEnv
  ]

--- ProgramCode ---

data ProgramCode
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
  | BachD        -- Datateknik
  | BachElec     -- Eelektronik
  | BachConst    -- Byggnadsteknik
  | BachKA       -- Kemisk analysteknik
  | BachM        -- Maskinteknik
  | BachBio      -- Biologi
  | BachExp      -- Experimentell och industriell biomedicin
  | BachCult     -- Kulturvetenskap
  | BachSoc      -- Samhällsplanering
  | BachSys      -- Systemvetenskap
  | BachChem     -- Kemisk biologi
  | BachCog      -- Kognitionsvetenskap
  | BachKSM      -- Kultur, samhhälle, mediagestalltning
  | BachIP       -- Innovativ programmering
  | BachAer      -- Flygtransport och logistik
  | BachLog      -- Samhällets logistik
  | BachMol      -- Kemi - molekylär design
  | BachNano     -- Fysik och nanovetenskap
  | BachMath     -- Matematik
  | BachGDK      -- Grafisk design och kommunikation
  | BachStat     -- Statistik och dataanalys
  | BachSC       -- Samhälls- och kulturanalys
  | BachPol      -- Policies
  | BachEnv      -- Miljövetare
  | BachLaw      -- Affärsjuridik
  | MasterDAV    -- Datavetenskap
  | MasterCS     -- Computer Science
  | MasterStat   -- Statistics and machine learning
  | MasterSoc    -- Samhällsgestaltning
  | MasterPS     -- Protein Science
  | MasterITSL   -- Intelligent Transport Systems and Logistics
  | MasterM      -- Mechanical Engineering
  | MasterHR     -- Human Resource Management and Development
  | MasterI      -- Industrial Engineering and Management
  | MasterEth    -- Masterprogram i Etnicitet och migration
  | MasterCom    -- Communication Systems
  | MasterAer    -- Aeronautical Engineering
  | MasterEco    -- Ecology and the Environment
  | MasterNano   -- Materials Science and Nanotechnology
  | MasterElec   -- Electronics Engineering
  | MasterPhys   -- Fysik och nanovetenskap
  | MasterAnim   -- Applied Ethology and Animal Biology
  | MasterLearn  -- Internationellt masterprogram i vuxnas lärande och...
  | MasterAeth   -- Applied ethics
  | MasterCog    -- Kognitionsvetenskap
  | MasterMed    -- Masterprogram i arbetsterapi/folkhälsovetenskap/...
  | MasterGen    -- Masterprogram i Intersektionell genusvetenskap som förändringsarbete
  | MasterChild  -- Child studies
  | MasterDes    -- Design
  | MasterMath   -- Matematik
  | MasterSus    -- Sustainability Engineering and Management
  | MasterBio    -- Biomedical Engineering
  | MasterEnv    -- Masterprogram i miljö- och utomhuspedagogik och friluftsliv
  deriving (Show, Read, Eq)

instance Arbitrary ProgramCode where
  arbitrary = genericArbitrary

--- ProgramSlug ---

data ProgramSlug
  = P6IDAT
  | P6IELK
  | P6IBYG
  | P6IKEA
  | P6IMAS
  | P6KBIO
  | PMGMB2
  | PF7KKU
  | PF7KSP
  | PF7KSY
  | P6KKEB
  | PF7KKO
  | PF7KKM
  | P6KIPR
  | P6KFTL
  | P6KLOG
  | P6KKEM
  | P6KFYN
  | P6KMAT
  | P6KGDK
  | PF7KSK
  | PF7KPO
  | PF7KSA
  | PF7KMO
  | PF7KAA

  -- Engineering
  | P6CDDD
  | P6CMJU
  | P6CIII
  | P6CIEI
  | P6CITE
  | P6CYYY
  | P6CYYI
  | P6CMED
  | P6CMEN
  | P6CIEN
  | P6CKTS
  | P6CMMM
  | P6CEMM
  | P6CTBI
  | P6CDPU
  | P6CKEB

  -- Master programs
  | P6MICS
  | P6MDAV
  | PF7MSL
  | PF7MSA
  | P6MPRO
  | P6MTSL
  | P6MMEC
  | PF7MHR
  | PF7MIO
  | PF7MEM
  | P6MCSY
  | P6MAER
  | P6MECO
  | P6MMSN
  | P6MELE
  | P6MFYS
  | P6METH
  | PL7MLG
  | PF7MKS
  | PMMIN1
  | PF7MGE
  | P6MDES
  | P6MMAT
  | P6MSUS
  | P6MBME
  | PL7MOS
  deriving (Show, Read, Eq)
-}
