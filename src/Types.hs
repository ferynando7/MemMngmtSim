module Types where

data BoolNum = Zero | One deriving (Eq, Show)
data Opening = First | Second deriving (Eq)
data Environment = Development | Production
