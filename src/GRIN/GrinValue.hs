{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds, EmptyDataDecls,  TypeInType, PolyKinds, TypeOperators, ExistentialQuantification, RankNTypes, DefaultSignatures,
GADTs, DuplicateRecordFields, PatternSynonyms, DeriveTraversable, DeriveGeneric, DeriveDataTypeable, DeriveLift, StandaloneDeriving, DeriveAnyClass #-}

module GRIN.GrinValue where

import Data.Kind
import GRIN.GrinTag
import Data.Data
import GRIN.GrinVariable
import GRIN.GrinIdentifiers
import GRIN.GrinSimpleValue
import Control.Lens.Plated
import Data.Data.Lens (uniplate)
import Control.Lens
import Control.Lens.Combinators


import Control.Applicative 


newtype Pointer ty = Pointer {locationName :: GrinVariable ty} deriving (Functor, Foldable, Traversable, Typeable, Show, Eq, Data)

-- | 'NodeType a' represents the type of GRIN node. A node can either have a known constant tag, a variable tag, or have an empty tag ('tagless'). Another name for a tagless inode is an unboxed node.  
data NodeType a where
  KnownTag :: {_knownTag :: GrinTag} -> NodeType a
  Tagless :: NodeType a
  VariableTag :: GrinVariable a -> NodeType a
  deriving ( Eq, Functor, Foldable, Traversable,  Typeable, Show)
deriving instance (Data a, Typeable a) => Data (NodeType a)


-- Have to extend this to include things such as original high-level type. Could use the LLVM format?
-- TODO: Remove EmptyValue from GrinValue and replace with pattern synonym. Likewise for PlainTag.
-- | A node is the basic object that goes on the heap. It is composed of a tag, and a (possibly empty) collection of fields.
-- Ideally, the codegen should be able to rearrange the fields to optimise memory access behaviour.
-- A tagless node without any fields is equivalent to an 'EmptyValue'.
data Node f a where
  Node ::  Traversable f => {_nodeType :: NodeType a, _fields ::  f (GrinValue f a)} -> Node f a
deriving instance (Show a, Show (f (GrinValue f a))) => Show (Node f a)


class WithKnownTag a where
  knownTag :: Prism' a GrinTag


instance (Alternative f, Traversable f) => WithKnownTag (Node f a) where
  knownTag = prism' (\a -> Node (KnownTag a) empty) (\a -> case a of
                                                   (BoxedNode t _) -> Just t
                                                   _ -> Nothing)


pattern BoxedNode tag fields = Node (KnownTag tag) fields
pattern UnboxedNode fields = Node Tagless fields


instance (Eq a, Eq (f a), Eq (f (GrinValue f a))) => Eq (Node f a) where
  (Node ty l) == (Node ty' l') = ty == ty' && l == l'

-- Just because all these fucking Data/Typeable constraints should die in a fire
type ValueConstraint f a = (Data a, Data (f (GrinValue f a)), Typeable f, Typeable a, Traversable f)

deriving instance (Functor f, Functor (GrinValue f)) => Functor (Node f)
deriving instance (Foldable f, Foldable (GrinValue f)) => Foldable (Node f)
deriving instance (Traversable f, Traversable (GrinValue f)) => Traversable (Node f)
deriving instance ValueConstraint f a => Data (Node f a)


data GrinValue f ty  where
  SimpleValue :: GrinSimpleValue ty -> GrinValue f ty
  EmptyValue :: GrinValue f ty
  PlainTag :: {t ::  GrinTag  } -> GrinValue f ty
  NodeValue :: Traversable f => Node f a -> GrinValue f a
--  BasicValue :: BasicValue ty -> GrinValue f ty
  PointerValue :: Pointer ty -> GrinValue f ty

instance (Alternative f, Traversable f) => WithKnownTag (GrinValue f ty) where
  knownTag = prism' PlainTag (\a -> case a of (PlainTag t) -> Just t
                                              (NodeValue n) -> n^?knownTag
                                              _ -> Nothing
                             )

deriving instance (Show ty, Show (f (GrinValue f ty))) => Show (GrinValue f ty)
deriving instance Typeable (GrinValue f ty)
deriving instance ValueConstraint f ty  => Data (GrinValue f ty)
deriving instance ValueConstraint f a => Plated (GrinValue f a) 
pattern Variable a = SimpleValue (VarValue (Var a))

deriving instance (Eq ty, Eq (f ty), Eq (f (GrinValue f ty))) => Eq (GrinValue f ty)
deriving instance Functor (GrinValue f)
deriving instance  Foldable (GrinValue f)
deriving instance Traversable (GrinValue f)

instance Applicative f => Applicative (GrinValue f)  where
  pure a = Variable a
  Variable f <*> a  = f <$> a

instance Applicative f => Monad (GrinValue f) where
  (Variable a)  >>= f = f a

