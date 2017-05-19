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
{-# LANGUAGE DeriveAnyClass #-}
module GRIN.GrinSimpleValue where

import Data.Type.Equality
import Data.Kind
import GRIN.GrinLiteral
import GRIN.GrinVariable
import Data.Data
import Control.Lens.Plated




data GrinSimpleValue a where
  Literal :: {value :: GrinLiteral} -> GrinSimpleValue a
  VarValue ::  GrinVariable a -> GrinSimpleValue a
  deriving (Show, Eq, Data, Typeable, Functor, Foldable, Traversable)


deriving instance Data a => Plated (GrinSimpleValue a)


instance Applicative GrinSimpleValue where
  pure a = VarValue (pure a)
  (VarValue f) <*>( VarValue a) = VarValue (f <*> a)

instance Monad GrinSimpleValue where
  (VarValue (Var a _)) >>= f = (f a)


