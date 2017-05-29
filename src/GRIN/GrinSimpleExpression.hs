 {-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Presburger #-}

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
import Data.OpenUnion
import Data.OpenUnion.Internal
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
  type SExprExt (exts) f = Union (MapList exts f)

                                                              
type family ConstrainedMembers constr (l :: [ * -> *]) = (r :: Constraint) | r -> l where
 ConstrainedMembers constr '[] = ()
 ConstrainedMembers constr (t ': c) = (constr t, ConstrainedMembers constr c)

type family ConstrainedMembers0 constr (l :: [* -> *]) = (r :: Constraint) | r -> l where
 ConstrainedMembers0 constr '[] = ()
 ConstrainedMembers0 constr (t ': c) = (ForallF constr t, ConstrainedMembers0 constr c)

deriving instance Typeable ([* -> *])
deriving instance Typeable '[]
deriving instance Typeable (h ': tail)
deriving instance Typeable Union

type family Is t (r :: [* -> *]) where
  Is h (h ': tail) = 0
  Is h (h' ': tail) = 1 + Is h tail

type family Length (l :: [* -> *]) = (r :: Nat) where
  Length '[] = 0
  Length (h ': tail) = 1 + Length tail

type family (a > b) :: Constraint where
  a > b =  (CmpNat a b) ~ GT

wrap :: forall (r :: [* -> *]) (a :: *) . (Typeable r, Typeable a) => Union r a -> Dynamic
wrap u = toDyn u

a :: Union [Maybe, Either Int, []] String
a = inj ["lol"]

bah = fmap toUpper a

type family ConstrAt constr r n = (res :: Constraint) where
  ConstrAt constr (h ': tail) 0 = constr h
  ConstrAt constr (h ': tail) n = ConstrAt constr tail (n-1)

type ConstrainThe h constr r  = ConstrAt constr r (Is h r)

{-instance {-#OVERLAPPING#-}forall  h r tail .(Functor h, r ~ (h ': tail), (Is h r ~ 0)) => Functor (Union (h ': tail)) where
  fmap :: forall a b. (a -> b) -> Union (h ': tail) a -> Union (h ': tail) b
  fmap f u@(Union 0 t) = let snip = decompd in Union 0 (fmap f (extract snip)) where
    decompd :: Union '[h] a
    decompd = case decomp u of
      Right t ->  inj t
      Left t -> error "fuck"
-}
instance  forall h r tail . (ConstrainedMembers Functor r, r ~ (h ': tail), Functor (Union tail),  Length r > 0) => Functor (Union r) where
  fmap :: forall a b . (a -> b) -> Union (h ': tail) a -> Union (h ': tail) b
  fmap f u@(Union 0 t) = let snip = decompd in Union 0 (fmap f (extract snip)) where
    decompd :: Union '[h] a
    decompd = case decomp u of
      Right t ->  inj t
      Left t -> error "fuck"
  fmap f u@(Union n t) = case decomp u of
    Left r -> let foo = fmap f r in weaken ( foo)

  {-
  fmap f u = weaken $ fmap f val where
    val :: Union tail a
    val = case decomp u of
      Left u' -> u'-}
{-
instance {-#OVERLAPPING#-} forall h r tail n. (r ~ (h ': tail), ConstrainThe h Functor r) => Functor (Union r) where
  fmap :: forall a b t. Functor t => (a -> b) -> Union (h ': tail) a -> Union (h ': tail) b
  fmap f u@(Union n t') = Union n (fmap f val) where
    decompd :: Union '[t] a
    decompd = case decomp u of
      Right t -> Unsafe.Coerce.unsafeCoerce (Union 0 t)
  -}    
--instance {-#OVERLAPPING#-}(ConstrainedMembers Functor r, r ~ (h ': tail),  (Length r) > 0) => Functor (Union r) where
--  fmap f (Union 0 t) = Union 0 (fmap f t)
{-  fmap f a@(Union n t) = case decomp a of
               Right t -> inj @(Union r ) (fmap f t)
               Left remainder -> weaken (fmap f remainder)
-}

instance  (ConstrainedMembers Foldable r, r ~ (h : tail), Foldable (Union tail), (Length r) > 0) => Foldable (Union r) where
  foldMap f a = case decomp a of
                  Right t -> foldMap f t
                  Left remainder ->  foldMap f remainder


instance (ConstrainedMembers Functor tail, ConstrainedMembers Foldable tail, ConstrainedMembers Traversable r, r ~ (h : tail), Traversable (Union tail), Length r > 0) => Traversable (Union r) where
  traverse (f :: a -> f b) t = case decomp t of
                   Right (a:: h a) -> pure undefined
                   Left remainder -> undefined (traverse f remainder)


instance  (Member (Identity) r, ConstrainedMembers Functor tail, ConstrainedMembers Applicative r, r ~ (h : tail), Applicative (Union tail), Length r > 0) => Applicative (Union r) where
  pure t = inj (Identity t)
  f' <*> a' = let a =  fromJust (prj a') in
    case decomp f' of
              Right f -> inj (f <*> ( a))

