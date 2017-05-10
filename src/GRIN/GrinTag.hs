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

newtype Arity = Arity {arityVal :: Integer} deriving (Eq, Num, Data, Typeable, Show)

instance Monoid Arity where
  mempty = Arity 0
  mappend = coerce ((+) :: Integer -> Integer -> Integer)


--data GrinTagType  = ConstructorTag  | FullyAppliedFunctionApplicationTag | PartialApplicationTag | NoTag deriving (Eq, Data, Typeable, Generic, Show)

pattern EnumTag name = Constructor name 0

-- | A 'GrinTag' can be thought of as being a type identifier in a Tarski-style universe with only product types.
-- Tags are what identify different node types. There are several kinds of tags, each representing different constructs; however, the different tag types are only a convention used by the user of GRIN and the codegen, for convenience and optimisation purposes.
-- In the GRIN language itself, tags are merely identifiers.
data GrinTag  where
  Constructor ::  {name' :: GrinIdentifier, arity' :: Arity} -> Tag -- ^ Indicates a fully applied constructor from the higher level language. 
  FunctionCall ::  { name' :: GrinIdentifier,  arity' :: Arity} -> Tag -- ^ Indicates a closure.
  PartialApplication :: {name' :: GrinIdentifier, arity' :: Arity} -> Tag -- ^ Indicates a partial closure.
  Hole :: {arity' :: Arity} -> Tag -- ^ We don't care what the tag is.
  RecTag :: Tag -- ^ Trying to figure out if this is actually needed.
  deriving (Data,  Typeable, Show, Eq)
--deriving instance Eq (GrinTag)


makeClassyFor "HasTag" "tag" [("name'", "tagName"), ("arity'", "arity")] ''GrinTag

