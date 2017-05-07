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

import Data.Text
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

import Control.Lens.TH as CLTH

data Boxing

data Pointer ty deriving (Functor, Foldable, Traversable, Typeable)
deriving instance Eq (Pointer ty)
deriving instance (Data ty, Typeable ty) => Data (Pointer ty)
data BasicValue ty deriving (Functor, Foldable, Traversable,  Typeable)
deriving instance Eq (BasicValue ty)
deriving instance (Data ty, Typeable ty) => Data (BasicValue ty)






data NodeType a where
  KnownTag :: {_knownTag :: GrinTag} -> NodeType a
  VariableTag :: GrinVariable a -> NodeType a
  deriving ( Eq, Functor, Foldable, Traversable,  Typeable)
deriving instance (Data a, Typeable a) => Data (NodeType a)
makeLenses  ''NodeType
makeClassyPrisms ''NodeType
data Node f a where
  Node ::  Traversable f => {_nodeType :: NodeType a, _fields ::  f (GrinValue f a)} -> Node f a

--nodeType :: Lens' (Node f a) (NodeType a)
--nodeType = id

--tag' :: Traversal' (Node f a) GrinTag
--tag' = _nodeType._NodeType._KnownTag



instance (Eq a, Eq (f a), Eq (f (GrinValue f a))) => Eq (Node f a) where
  (Node ty l) == (Node ty' l') = ty == ty' && l == l'


type ValueConstraint f a = (Data a, Data (f (GrinValue f a)), Typeable f, Typeable a, Traversable f)

deriving instance (Functor f, Functor (GrinValue f)) => Functor (Node f)
deriving instance (Foldable f, Foldable (GrinValue f)) => Foldable (Node f)
deriving instance (Traversable f, Traversable (GrinValue f)) => Traversable (Node f)
deriving instance ValueConstraint f a => Data (Node f a)


data GrinValue f ty  where
  SimpleValue :: SVal ty -> GrinValue f ty
  EmptyValue :: GrinValue f ty
  PlainTag :: {tag ::  GrinTag  } -> GrinValue f ty
  NodeValue :: Traversable f => Node f a -> GrinValue f a
  BasicValue :: BasicValue ty -> GrinValue f ty
  PointerValue :: Pointer ty -> GrinValue f ty
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




type SVal = GrinSimpleValue


makeClassy  ''Node

--instance HasTag (Node f a) where

