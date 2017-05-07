{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds, TypeInType #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module GRIN.GrinSimpleValue where

import Data.Type.Equality
import Data.Kind
import GRIN.GrinLiteral
import GRIN.GrinVariable
import Data.Data
import GHC.Generics
data LiteralValue = LV deriving (Eq, Data, Typeable)

data GrinSimpleValue a where
  Literal :: {value :: GrinLiteral} -> GrinSimpleValue a
  VarValue ::  GrinVariable a -> GrinSimpleValue a

deriving instance (Data a, Typeable a) => Data (GrinSimpleValue a)

deriving instance Eq a => Eq (GrinSimpleValue a)




instance Functor GrinSimpleValue where
  fmap f (VarValue a) = VarValue (fmap f a)

instance Applicative GrinSimpleValue where
  pure a = VarValue (pure a)
  (VarValue f) <*> (VarValue a) = VarValue (f <*> a)

instance Monad GrinSimpleValue where
  (VarValue (Var a)) >>= f = (f a)


instance Foldable GrinSimpleValue where
  foldMap f (VarValue a) = foldMap f a



instance Traversable GrinSimpleValue where
  traverse f (VarValue a) = VarValue <$> traverse f a
