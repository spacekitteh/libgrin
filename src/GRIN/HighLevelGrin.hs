{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds, EmptyDataDecls,  TypeInType, TypeOperators, ExistentialQuantification, RankNTypes, DefaultSignatures,
GADTs, DuplicateRecordFields, PatternSynonyms, DeriveTraversable, DeriveGeneric, DeriveDataTypeable, DeriveLift, StandaloneDeriving, DeriveAnyClass #-}

module GRIN.HighLevelGrin where

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

data HighLevelSimpleExpression ext f a where
    Store :: {value :: Val f a} -> HighLevelSimpleExpression ext f a -- ^ Equivalent to an 'Alloc' followed by an 'UpdateUnit'.
    Eval :: {name :: Name} -> HighLevelSimpleExpression ext f a -- ^ When we inline Eval, we can create a giant literal 'eval' method, but a better way is to generate specialised 'eval' methods for each use of 'Eval'.
    Apply :: Traversable f => {name :: Name, args :: f (Val f a)} -> HighLevelSimpleExpression ext f a -- ^ Returns either a partial application node or a closure. Either way, very simple to inline.
    FetchNodeX :: {name :: Name} -> HighLevelSimpleExpression ext f a -- ^ Fetches the node at 'name'. Equivalent to many FetchField's followed by a Unit to create the node.
    deriving (Typeable)

data HighLevelGrin

deriving instance (ValueConstraint f a) => Data (HighLevelSimpleExpression HighLevelGrin f a)
deriving instance Functor (HighLevelSimpleExpression ext f)
deriving instance Foldable (HighLevelSimpleExpression ext f)
deriving instance Traversable (HighLevelSimpleExpression ext f)
deriving instance (Show a, Show (f (Val f a))) => Show (HighLevelSimpleExpression ext f a)
