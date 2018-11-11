module Types where

data BoolNum = Zero | One | NAN deriving (Eq, Show)
data Opening = First | Second deriving (Eq)
data Environment = Development | Production deriving (Eq)

data Debug = NoDebug | Debug {getDLineNumber :: Integer,
                    getDPhysicalMemory :: Integer,
                    getDProcessId :: Integer,
                    getDFrameNumber :: Integer,
                    getDDirtyBit :: BoolNum} deriving (Eq)

instance Show Debug where
    show debug = unwords [show $ getDLineNumber debug, show $ getDPhysicalMemory debug, show $ getDProcessId debug, show $ getDFrameNumber debug, show $ getDDirtyBit debug]
