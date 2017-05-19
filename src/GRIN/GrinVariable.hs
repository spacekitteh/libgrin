{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds, EmptyDataDecls,  TypeInType, TypeOperators, ExistentialQuantification, RankNTypes, DefaultSignatures, PolyKinds, DataKinds,
GADTs, DuplicateRecordFields, PatternSynonyms, DeriveTraversable, DeriveGeneric, DeriveDataTypeable, DeriveLift, StandaloneDeriving, NoMonomorphismRestriction #-}
module GRIN.GrinVariable where
import Data.Data
import Data.Text
import Control.Lens
import Control.Lens.Plated
import Data.Kind
data GrinVariable uniqueTy where
  Var :: { _v ::uniqueTy, friendlyName :: Maybe Text} -> GrinVariable uniqueTy
  Hole :: GrinVariable uniqueTy
  deriving (Show, Functor, Foldable, Traversable, Data, Typeable)


data VariableX ext a where
  VarX :: XVar ext a -> a -> VariableX ext a
  VariableX :: XVariableX ext a -> VariableX ext a


type family (XVar ext a) 
type family XVariableX ext a

{-
How to make this composable? I was thinking something of using an open union but I'm not quite sure of what my desired functionality is.

Do I want to just keep injecting various annotations? Or do I want to remove some as well?

Should I basically be using Effects for the annotations? One might argue that.
-}


pattern NamelessVariable a = Var a Nothing


instance Eq uniqueTy => Eq (GrinVariable uniqueTy) where
  Var {_v = v1} == Var {_v = v2} = v1 == v2
  _ == _ = False

instance (Each (GrinVariable ty) (GrinVariable ty1) ty ty1) where
instance Applicative (GrinVariable) where
  pure t = Var t Nothing
  Var f _ <*> Var t n= Var (f t) n
  Var f _ <*> Hole = Hole

instance Monad GrinVariable where
  Var a _ >>= f = f a

instance Plated (GrinVariable ty) where
  plate f a = f a

class BindsVariables a  where
  boundVariables :: Prism' (a ty) (GrinVariable ty)


variable :: Prism' (GrinVariable ty) (GrinVariable ty)
variable = prism' id ( \a ->  case a of
                                 Var t _ -> Just a
                                 Hole -> Nothing
                     )

instance BindsVariables (GrinVariable ) where
  boundVariables = variable
