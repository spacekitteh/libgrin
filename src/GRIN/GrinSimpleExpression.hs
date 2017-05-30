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
type VariableName = GrinIdentifier
type FunctionName = GrinIdentifier


















data GrinSimpleExprX (ext :: k) (f :: * -> *) a where
  UnitX :: Traversable f => {value :: GrinValue f a } -> GrinSimpleExprX ext f a -- ^ Returns 'value'.
  UpdateX :: {name :: VariableName, value :: GrinValue f a} -> GrinSimpleExprX ext f a -- ^ Updates the node pointed to by 'name' with 'value'. Returns ().
  CallX :: {name :: FunctionName, args :: f GrinIdentifier} -> GrinSimpleExprX ext f a -- ^ Calls a function 'name' with arguments 'args'.
  GrinSimpleExprExt ::  SExprExt ext f a -> GrinSimpleExprX ext f a
{-  FFI :: {name :: Name, callingConvention :: CallConvention, impEnt :: ForeignEnt,
       ffiAnnot :: FFIAnnotation, args :: Arguments} -> SExpr-}
  deriving Typeable


type ExtConstraint f = (Traversable f, Monad f)

deriving instance forall ext f . Functor (SExprExt ext f) => Functor (GrinSimpleExprX ext f)
deriving instance forall ext f . Foldable (SExprExt ext f) => Foldable (GrinSimpleExprX ext f)
deriving instance forall ext f . Traversable (SExprExt ext f) => Traversable (GrinSimpleExprX ext f)

instance (Applicative (SExprExt ext f), Traversable f) => Applicative (GrinSimpleExprX ext f) where
  pure a = UnitX (Variable a Nothing)
  UnitX (Variable f n) <*> a = (f <$> a)

class   SimpleExprExtension ext (f:: * -> *) a  where
  type SExprExt ext f = (r :: * -> *)  | r -> ext
type family SExprExtConstraint ext f (constr :: (* -> *) -> Constraint) = (r :: Constraint) where -- | r -> ext constr where
  SExprExtConstraint ext f constr = constr (SExprExt ext f)


type family MapList (l :: [ * -> * ]) (f :: * -> *)  = (r :: [* -> *] ) | r -> l  where
  MapList '[] f = ('[] :: [* -> *])
  MapList (ext ': rest) f = (SExprExt ext f) ': (MapList rest f)


type family MapConstraint constr (l :: [ * -> *]) (f :: * -> *) = (r :: Constraint) where -- | r -> l where-- 
  MapConstraint constr '[] f = ()
  MapConstraint constr (ext ': rest) f = (constr (SExprExt ext f), MapConstraint constr rest f)


instance (Length exts > 0) =>SimpleExprExtension (exts :: [* -> *]) f a where
  type SExprExt (exts) f = Union (Traversable :&&: Monad) (MapList exts f)

                                                              
type family ConstrainedMembers constr (l :: [ * -> *]) = (r :: Constraint) | r -> l where
 ConstrainedMembers constr '[] = ()
 ConstrainedMembers constr (t ': c) = (constr t, ConstrainedMembers constr c)

type family ConstrainedMembers0 constr (l :: [* -> *]) = (r :: Constraint) | r -> l where
 ConstrainedMembers0 constr '[] = ()
 ConstrainedMembers0 constr (t ': c) = (ForallF constr t, ConstrainedMembers0 constr c)



type family Is t (r :: [* -> *]) where
  Is h (h ': tail) = 0
  Is h (h' ': tail) = 1 + Is h tail

type family Length (l :: [* -> *]) = (r :: Nat) where
  Length '[] = 0
  Length (h ': tail) = 1 + Length tail

type family (a > b) :: Constraint where
  a > b =  (CmpNat a b) ~ GT



type family ConstrAt constr r n = (res :: Constraint) where
  ConstrAt constr (h ': tail) 0 = constr h
  ConstrAt constr (h ': tail) n = ConstrAt constr tail (n-1)

type ConstrainThe h constr r  = ConstrAt constr r (Is h r)

