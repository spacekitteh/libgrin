 {-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Presburger #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds , PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds, EmptyDataDecls,  TypeInType, TypeOperators, ExistentialQuantification, RankNTypes, DefaultSignatures, TypeApplications,
GADTs, DuplicateRecordFields, PatternSynonyms, DeriveTraversable, DeriveGeneric, DeriveDataTypeable, DeriveLift, StandaloneDeriving, DeriveAnyClass , FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables #-}

module GRIN.GrinSimpleExpression where
import qualified Data.Map.Strict as Map
import Data.Text
import GHC.Generics
import Data.Data
import Data.Kind
import Data.Constraint
import Control.Lens.Plated
import Data.Either
import GRIN.GrinIdentifiers
import Data.Dynamic
import GRIN.GrinLiteral
import GRIN.GrinVariable
import GRIN.GrinValue
import GRIN.GrinTag
import GRIN.GrinCase
import GRIN.GrinSimpleValue
import Data.Coerce
import Data.Constraint.Lifting
import Data.Constraint.Forall
import Union
--import Data.OpenUnion
--import Data.OpenUnion.Internal
import Control.Applicative
import Data.Functor.Identity
import qualified Unsafe.Coerce
import Data.Maybe (fromJust)
import GHC.TypeLits hiding (type (*))
import Data.Functor.Compose
import Numeric.Natural
type VariableName = GrinIdentifier
type FunctionName = GrinIdentifier

type FieldOffset = Natural

data StackOrHeap = Stack | Heap deriving (Data, Show, Eq)


{-type Val a = GrinValue [] a

data GrinSimpleExpression a where
  Unit :: Val a -> GrinSimpleExpression a
  Update :: VariableName -> Val a -> GrinSimpleExpression a
  Call :: FunctionName -> [GrinIdentifier] -> GrinSimpleExpression a
  -- High level
  Store :: Val a -> GrinSimpleExpression a
  Eval :: VariableName -> GrinSimpleExpression a
  Apply :: VariableName -> [Val a] -> GrinSimpleExpression a
  FetchNode :: VariableName -> GrinSimpleExpression a
  -- Low level
  Alloc :: Natural -> Maybe StackOrHeap -> GrinSimpleExpression a
  Free :: Pointer a -> GrinSimpleExpression a
  FetchUpdate :: {source :: VariableName, target :: VariableName} -> GrinSimpleExpression a
  FetchField :: VariableName -> FieldOffset -> Maybe Tag -> GrinSimpleExpression a
  
  deriving (Eq, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)


instance Applicative (GrinSimpleExpression) where
  pure a = Unit (Variable a Nothing)
  Unit (Variable f n) <*> a = f <$> a


-}

  

data GrinSimpleExprX (ext :: k) (f :: * -> *) a where
  UnitX :: Traversable f => {value :: GrinValue f a } -> GrinSimpleExprX ext f a -- ^ Returns 'value'.
  UpdateX :: {name :: VariableName, value :: GrinValue f a} -> GrinSimpleExprX ext f a -- ^ Updates the node pointed to by 'name' with 'value'. Returns ().
  CallX :: {name :: FunctionName, args :: f GrinIdentifier} -> GrinSimpleExprX ext f a -- ^ Calls a function 'name' with arguments 'args'.
  GrinSimpleExprExt ::  SExprExt ext f a -> GrinSimpleExprX ext f a
{-  FFI :: {name :: Name, callingConvention :: CallConvention, impEnt :: ForeignEnt,
       ffiAnnot :: FFIAnnotation, args :: Arguments} -> SExpr-}
  deriving (Typeable)


type ExtConstraint f = (Traversable f, Monad f)

deriving instance forall ext f . Functor (SExprExt ext f) => Functor (GrinSimpleExprX ext f)
deriving instance forall ext f . Foldable (SExprExt ext f) => Foldable (GrinSimpleExprX ext f)
deriving instance forall ext f . Traversable (SExprExt ext f) => Traversable (GrinSimpleExprX ext f)

instance forall ext f a. (Show a, Show (f (GrinValue f a)), Show (f GrinIdentifier), Show (SExprExt ext f a)) => Show (GrinSimpleExprX ext f a) where
  show (UnitX v) = "unit " ++ show v
  show (UpdateX name value) = "update " ++ show name ++ " <- " ++ show value
  show (CallX name args) = "call " ++ show name ++ " with " ++ show args
  show (GrinSimpleExprExt s) = show s
  


instance (Functor (SExprExt ext f), Traversable f) => Applicative (GrinSimpleExprX ext f) where
  pure a = UnitX (Variable a Nothing)
  UnitX (Variable f n) <*> a = (f <$> a)

class   SimpleExprExtension ext (f:: * -> *) a  where
  type SExprExt ext f = (r :: * -> *)  | r -> ext
type family SExprExtConstraint ext f (constr :: (* -> *) -> Constraint) = (r :: Constraint) where -- | r -> ext constr where
  SExprExtConstraint ext f constr = constr (SExprExt ext f)


type family MapList (l :: [ * -> * ]) (f :: * -> *)  = (r :: [* -> *] ) | r -> l  where
  MapList '[] f = ('[] :: [* -> *])
  MapList (ext ': rest) f = (SExprExt ext f) ': (MapList rest f)







instance (Length exts > 0) =>SimpleExprExtension (exts :: [* -> *]) f a where
  type SExprExt (exts) f =  ( Union (Traversable ) (MapList exts f))

                                                              
type family Length (l :: [* -> *]) = (r :: Nat) where
  Length '[] = 0
  Length (h ': tail) = 1 + Length tail

type family (a > b) :: Constraint where
  a > b =  (CmpNat a b) ~ GT



type family ConstrAt constr r n = (res :: Constraint) where
  ConstrAt constr (h ': tail) 0 = constr h
  ConstrAt constr (h ': tail) n = ConstrAt constr tail (n-1)


