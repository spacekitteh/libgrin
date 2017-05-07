{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds, EmptyDataDecls,  TypeInType, TypeOperators, ExistentialQuantification, RankNTypes, DefaultSignatures,
GADTs, DuplicateRecordFields, PatternSynonyms, DeriveTraversable, DeriveGeneric, DeriveDataTypeable, DeriveLift, StandaloneDeriving #-}
module GRIN.GrinVariable where
import Data.Data
import Control.Lens.TH
import Control.Lens.Plated
data GrinVariable ty where
  Var :: {_v ::ty} -> GrinVariable ty
  Hole :: GrinVariable ty
  deriving Data
deriving instance Eq ty => Eq (GrinVariable ty)

instance Functor (GrinVariable) where
  fmap f (Var t)  = Var (f t)

instance Applicative (GrinVariable) where
  pure t = Var t
  Var f <*> Var t = Var (f t)

instance Monad GrinVariable where
  Var a >>= f = f a

deriving instance Foldable GrinVariable
deriving instance Traversable GrinVariable

instance Data ty => Plated (GrinVariable ty) where

makeClassyFor "HasVariable" "variable" [("_v","rawVariable")]''GrinVariable 
