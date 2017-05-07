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
import Control.Lens.TH
import Data.Coerce
type Tag = GrinTag 

newtype Arity = Arity {arityVal :: Integer} deriving (Eq, Num, Data, Typeable)

instance Monoid Arity where
  mempty = Arity 0
  mappend = coerce ((+) :: Integer -> Integer -> Integer)


data GrinTagType  = ConstructorTag  | FullyAppliedFunctionApplicationTag | PartialApplicationTag | NoTag deriving (Eq, Data, Typeable, Generic)

pattern EnumTag name = Constructor name 0

data GrinTag  where
  Constructor ::  {name' :: GrinIdentifier, arity' :: Arity} -> Tag
  FunctionCall ::  { name' :: GrinIdentifier,  arity' :: Arity} -> Tag
  PartialApplication :: {name' :: GrinIdentifier, arity' :: Arity} -> Tag
  Unboxed :: Tag 
  Hole :: {arity' :: Arity} -> Tag 
  RecTag :: Tag 
  deriving (Data,  Typeable)
deriving instance Eq (GrinTag)


makeClassyFor "HasTag" "tag" [("name'", "tagName"), ("arity'", "arity")] ''GrinTag

