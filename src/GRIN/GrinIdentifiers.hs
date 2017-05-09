{-# LANGUAGE DeriveDataTypeable #-}
module GRIN.GrinIdentifiers where

import Data.Text
import Data.Data
data GrinIdentifier = FunctionName Text
                    | ConstructorName Text
                    | ArgumentName Text
                    | NodeName Text
                    | ModuleName Text
                    | VariableName Text
                    | GlobalName Text
                    deriving (Eq, Data, Typeable, Show)
