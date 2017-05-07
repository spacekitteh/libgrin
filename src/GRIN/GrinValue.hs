{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds, EmptyDataDecls,  TypeInType, PolyKinds, TypeOperators, ExistentialQuantification, RankNTypes, DefaultSignatures,
GADTs, DuplicateRecordFields, PatternSynonyms, DeriveTraversable, DeriveGeneric, DeriveDataTypeable, DeriveLift, StandaloneDeriving #-}

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
type Val = GrinValue

data Boxing

data Pointer ty deriving (Functor, Foldable, Traversable, Typeable)
deriving instance Eq (Pointer ty)
deriving instance (Data ty, Typeable ty) => Data (Pointer ty)
data BasicValue ty deriving (Functor, Foldable, Traversable,  Typeable)
deriving instance Eq (BasicValue ty)
deriving instance (Data ty, Typeable ty) => Data (BasicValue ty)






data NodeType a where
  KnownTag :: GrinTag -> NodeType a
  VariableTag :: GrinVariable a -> NodeType a
  deriving ( Eq, Functor, Foldable, Traversable,  Typeable)
deriving instance (Data a, Typeable a) => Data (NodeType a)

data Node f a where
  Node ::  Traversable f => NodeType a ->  f (GrinValue f a) -> Node f a

instance (Eq a, Eq (f a), Eq (f (GrinValue f a))) => Eq (Node f a) where
  (Node ty l) == (Node ty' l') = ty == ty' && l == l'


deriving instance (Functor f, Functor (GrinValue f)) => Functor (Node f)
deriving instance (Foldable f, Foldable (GrinValue f)) => Foldable (Node f)
deriving instance (Traversable f, Traversable (GrinValue f)) => Traversable (Node f)
deriving instance (Data a, Data (f (GrinValue f a)), Typeable f, Typeable a, Traversable f) => Data (Node f a)


data GrinValue f ty  where
  SimpleValue :: SVal ty -> Val f ty
  EmptyValue :: Val f ty
  PlainTag :: {tag ::  GrinTag  } -> Val f ty
  NodeValue :: Traversable f => Node f a -> GrinValue f a
  BasicValue :: BasicValue ty -> Val f ty
  PointerValue :: Pointer ty -> Val f ty
deriving instance Typeable (GrinValue f ty)
deriving instance (Typeable f, Typeable ty, Data (f ty), Data ty, Data (f (GrinValue f ty)), Traversable f) => Data (GrinValue f ty)

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

instance (Data a, Typeable f, Data (f a), Data (f (GrinValue f a)), Traversable f) => Plated (GrinValue f a) 


type SVal = GrinSimpleValue

