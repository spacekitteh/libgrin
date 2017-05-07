{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds, EmptyDataDecls,  TypeInType, TypeOperators, ExistentialQuantification, RankNTypes, DefaultSignatures,
GADTs, DuplicateRecordFields, PatternSynonyms, DeriveTraversable, DeriveGeneric, DeriveDataTypeable, DeriveLift, StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module GRIN.GrinTag where
import Data.Data
import Data.Typeable
import GHC.Generics
import GHC.TypeLits
import GRIN.GrinIdentifiers
type Tag = GrinTag 

newtype Arity = Arity {arityVal :: Integer} deriving (Eq, Num, Data, Typeable)



data GrinTagType  = ConstructorTag  | FullyAppliedFunctionApplicationTag | PartialApplicationTag | NoTag deriving (Eq, Data, Typeable, Generic)

pattern EnumTag name = Constructor name 0

data GrinTag  where
  Constructor ::  ConstructorName -> Arity -> Tag
  FunctionCall ::  FunctionName -> Arity -> Tag
  PartialApplication :: FunctionName -> Arity -> Tag
  Unboxed :: Tag 
  Hole :: Tag 
  RecTag :: Tag 
  deriving (Data,  Typeable)
deriving instance Eq (GrinTag)

--deriving instance Data (GrinTag t)
