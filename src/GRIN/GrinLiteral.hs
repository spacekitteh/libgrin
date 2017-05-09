{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module GRIN.GrinLiteral where

import Data.Text
import Data.Data

data GrinLiteral  =
    LitInteger Integer
  | LitFloat Float
  | LitDouble Double
  | LitChar Char
  | LitBool Bool
  | LitNull
  | LitString String
  | LitLLVM
  deriving (Eq, Ord, Data, Typeable, Show)
