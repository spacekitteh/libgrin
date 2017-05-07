{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module GRIN.GrinLiteral where

import Data.Text
import Data.Data


data GrinLiteral where
  StringLiteral :: Text -> GrinLiteral
  IntegerLiteral :: Integer -> GrinLiteral
  deriving (Eq, Ord, Data, Typeable)
