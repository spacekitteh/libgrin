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
data Boxing

data Pointer ty = P deriving (Functor, Foldable, Traversable, Typeable, Show, Eq, Data)


data NodeType a where
  KnownTag :: {_knownTag :: GrinTag} -> NodeType a
  Tagless :: NodeType a
  VariableTag :: GrinVariable a -> NodeType a
  deriving ( Eq, Functor, Foldable, Traversable,  Typeable, Show)
deriving instance (Data a, Typeable a) => Data (NodeType a)

data Node f a where
  Node ::  Traversable f => {_nodeType :: NodeType a, _fields ::  f (GrinValue f a)} -> Node f a
deriving instance (Show a, Show (f (GrinValue f a))) => Show (Node f a)

class KnownTaggedNode a where
  knownTag :: Prism' a GrinTag


instance (Alternative f, Traversable f) => KnownTaggedNode (Node f a) where
  knownTag = prism' (\a -> Node (KnownTag a) empty) (\a -> case a of
                                                   (BoxedNode t _) -> Just t
                                                   _ -> Nothing)


pattern BoxedNode tag fields = Node (KnownTag tag) fields
pattern UnboxedNode fields = Node Tagless fields


instance (Eq a, Eq (f a), Eq (f (GrinValue f a))) => Eq (Node f a) where
  (Node ty l) == (Node ty' l') = ty == ty' && l == l'


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

instance (Alternative f, Traversable f) => KnownTaggedNode (GrinValue f ty) where
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

