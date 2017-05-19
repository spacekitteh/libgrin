{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds, EmptyDataDecls,  TypeInType, TypeOperators, ExistentialQuantification, RankNTypes, DefaultSignatures,
GADTs, DuplicateRecordFields, PatternSynonyms, DeriveTraversable, DeriveGeneric, DeriveDataTypeable, DeriveLift, StandaloneDeriving, DeriveAnyClass #-}

module GRIN.LowLevelGrin where

import qualified Data.Map.Strict as Map
import Data.Text
--import GHC.TypeLits
import GHC.Generics
import Data.Data
import Data.Kind
import GRIN.GrinValue
import Control.Lens.Plated
import GRIN.GrinIdentifiers
import GRIN.GrinValue
import GRIN.GrinTag

type Val = GrinValue
type Name = GrinIdentifier
type VariableName = GrinIdentifier
type FunctionName = GrinIdentifier


type FieldOffset = Int

type ModuleName = GrinIdentifier


-- ^ Low-level GRIN instructions. These allow for more fine-grained optimisations and easier translation to the next language in the pipeline, especially LLVM.
data LowLevelSimpleExpression ext f a where
  Alloc :: {size :: Val f a} -> LowLevelSimpleExpression ext f a -- ^ Ask for a pointer to a store in the heap with maximum size 'size'.
  Dealloc :: {target :: Pointer a} -> LowLevelSimpleExpression ext f a -- ^ We can insert Deallocs when the variable is dead and nothing points to its value (from the sharing analysis)
  FetchUpdate :: {source :: Name, destination :: Name} -> LowLevelSimpleExpression ext f a -- ^ Copies the node pointed to by 'source' and puts it in 'destination'.
  FetchField :: {name :: Name, offset :: FieldOffset, tag ::  Maybe Tag } -> LowLevelSimpleExpression ext f a -- ^ Fetch individual fields, rather than the entire node. Offset 0 is the tag.
  deriving  (Typeable)

data LowLevelGrin

deriving instance (ValueConstraint f a) => Data (LowLevelSimpleExpression LowLevelGrin f a)
deriving instance Functor (LowLevelSimpleExpression ext f)
deriving instance Foldable (LowLevelSimpleExpression ext f)
deriving instance Traversable (LowLevelSimpleExpression ext f)
deriving instance (Show a, Show (f (Val f a))) => Show (LowLevelSimpleExpression ext f a)
