{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds, EmptyDataDecls,  TypeInType, TypeOperators, ExistentialQuantification, RankNTypes, DefaultSignatures,
GADTs, DuplicateRecordFields, PatternSynonyms, DeriveTraversable, DeriveGeneric, DeriveDataTypeable, DeriveLift, StandaloneDeriving, DeriveAnyClass , FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables #-}

module GRIN.GrinSimpleExpression where
import qualified Data.Map.Strict as Map
import Data.Text
--import GHC.TypeLits
import GHC.Generics
import Data.Data
import Data.Kind
import Data.Constraint
import Control.Lens.Plated
import GRIN.GrinIdentifiers
import GRIN.GrinLiteral
import GRIN.GrinVariable
import GRIN.GrinValue
import GRIN.GrinTag
import GRIN.GrinCase
import GRIN.GrinSimpleValue
import Data.Coerce
import Data.OpenUnion
import Data.OpenUnion.Internal

type VariableName = GrinIdentifier
type FunctionName = GrinIdentifier

data GrinSimpleExprX (ext::k) f a where
  UnitX :: Traversable f => {value :: GrinValue f a } -> GrinSimpleExprX ext f a -- ^ Returns 'value'.
  UpdateX :: {name :: VariableName, value :: GrinValue f a} -> GrinSimpleExprX ext f a -- ^ Updates the node pointed to by 'name' with 'value'. Returns ().
  CallX :: {name :: FunctionName, args :: f GrinIdentifier} -> GrinSimpleExprX ext f a -- ^ Calls a function 'name' with arguments 'args'.
  GrinSimpleExprExt :: Traversable f => GrinSimpleExprExtType ext f a -> GrinSimpleExprX ext f a
{-  FFI :: {name :: Name, callingConvention :: CallConvention, impEnt :: ForeignEnt,
       ffiAnnot :: FFIAnnotation, args :: Arguments} -> SExpr-}
  deriving Typeable


type family  GrinSimpleExprExtType (ext::k) (f:: * -> *) a = (r :: *)   where
  GrinSimpleExprExtType ext f a = GrinSimpleExprExtType1 ext f a


type family GrinSimpleExprExtConstraint (constr :: * -> Constraint) ext (f :: * -> *) a = (r :: Constraint)  where
  GrinSimpleExprExtConstraint constr ext f a = constr (GrinSimpleExprExtType ext f a)


type family (GrinSimpleExprExtType1 (ext :: k) (f:: * -> * )  ) = (r :: * -> *) 

type family MapList (l :: [ * -> * ]) f  = (r :: [* -> *] )  where
  MapList '[ext] f = '[GrinSimpleExprExtType1 ext f]
  MapList (ext ': rest) f = (GrinSimpleExprExtType1 ext f) : MapList rest f

type instance GrinSimpleExprExtType1 (Union (ext :: [ * -> * ])) f  = Union (MapList ext f)




                                                              
type family ConstrainedMembers constr l = (r :: Constraint) where
  ConstrainedMembers constr (t ': c) = (constr t, ConstrainedMembers constr c)
  ConstrainedMembers constr '[] = ()



instance {-#OVERLAPPING #-} Functor (Union '[]) where
  fmap f a = error "Absurd"
instance {-#OVERLAPPING #-}(ConstrainedMembers Functor r,  r ~ (h : tail), Functor (Union tail)) => Functor (Union r) where
  fmap f a = case decomp a of
               Right t -> inj (fmap f t)
               Left remainder -> weaken (fmap f remainder)




type family GrinSimpleExprExtConstraint1 (constr :: (* -> *) -> Constraint) ext (f :: * -> *) = (r :: Constraint)  where
  GrinSimpleExprExtConstraint1 constr ext f = constr (GrinSimpleExprExtType1 ext f)



deriving instance (Show a, Show (f (GrinValue f a)), Show (f GrinIdentifier), Show (GrinSimpleExprExtType ext f a)) => Show (GrinSimpleExprX ext f a)

instance GrinSimpleExprExtConstraint1 Functor ext f => Functor (GrinSimpleExprX ext f) where
  fmap f (UnitX v) = UnitX (fmap f v)
  fmap f (UpdateX n v) = UpdateX n (fmap f v)
  fmap f (GrinSimpleExprExt a) = GrinSimpleExprExt (fmap f a)
  fmap f (CallX n args) = CallX n args

instance GrinSimpleExprExtConstraint1 Foldable ext f => Foldable (GrinSimpleExprX ext f) where
  foldMap m (UnitX v) = foldMap m v
  foldMap m (UpdateX n v) = foldMap m v
  foldMap m (CallX _ _) = mempty
  foldMap m (GrinSimpleExprExt e) = foldMap m e

instance GrinSimpleExprExtConstraint1 Traversable ext trav => Traversable (GrinSimpleExprX ext trav) where
  traverse :: Applicative f => (a -> f b) -> GrinSimpleExprX ext trav a -> f (GrinSimpleExprX ext trav b)
  traverse f (UnitX v) =  UnitX <$> (traverse f v)
  traverse f (UpdateX n v) = UpdateX n  <$> (traverse f v)
  traverse f (CallX n a) = pure (CallX n a)
  traverse f (GrinSimpleExprExt a) = GrinSimpleExprExt <$> traverse f a


deriving instance forall k (ext::k) f a. (Typeable k, ValueConstraint f a, Data (f GrinIdentifier), GrinSimpleExprExtConstraint Data ext f a, Typeable ext)  => Data (GrinSimpleExprX ext f a)

instance (Traversable f, GrinSimpleExprExtConstraint1 Functor ext f) => Applicative (GrinSimpleExprX ext f) where
  pure a = UnitX (Variable a Nothing)
  UnitX (Variable f n) <*> a = (f <$> a)

