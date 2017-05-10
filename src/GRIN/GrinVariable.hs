{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds, EmptyDataDecls,  TypeInType, TypeOperators, ExistentialQuantification, RankNTypes, DefaultSignatures,
GADTs, DuplicateRecordFields, PatternSynonyms, DeriveTraversable, DeriveGeneric, DeriveDataTypeable, DeriveLift, StandaloneDeriving, NoMonomorphismRestriction #-}
module GRIN.GrinVariable where
import Data.Data
import Control.Lens
import Control.Lens.Plated
data GrinVariable ty where
  Var :: {_v ::ty} -> GrinVariable ty
  Hole :: GrinVariable ty
  deriving (Data, Show, Eq, Functor, Foldable, Traversable)

instance (Each (GrinVariable ty) (GrinVariable ty1) ty ty1) where
instance Applicative (GrinVariable) where
  pure t = Var t
  Var f <*> Var t = Var (f t)
  Var f <*> Hole = Hole

instance Monad GrinVariable where
  Var a >>= f = f a

instance Data ty => Plated (GrinVariable ty) where

--makeClassyFor "HasVariable" "variable" [("_v","rawVariable")]''GrinVariable

class BindsVariables a  where
  boundVariables :: Prism' (a ty) (GrinVariable ty)


variable :: Prism' (GrinVariable ty) (GrinVariable ty)
variable = prism' id ( \a ->  case a of
                                 Var t -> Just a
                                 Hole -> Nothing
                     )

instance BindsVariables (GrinVariable ) where
  boundVariables = variable
