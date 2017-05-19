{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds, EmptyDataDecls,  TypeInType, TypeOperators, ExistentialQuantification, RankNTypes, DefaultSignatures,
GADTs, DuplicateRecordFields, PatternSynonyms, DeriveTraversable, DeriveGeneric, DeriveDataTypeable, DeriveLift, StandaloneDeriving, DeriveAnyClass #-}

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


type family  GrinSimpleExprExtType (ext::k) (f:: * -> *) a = (r :: *) | r -> ext  where
  GrinSimpleExprExtType ext f a = GrinSimpleExprExtType1 ext f a


type family GrinSimpleExprExtConstraint (constr :: * -> Constraint) ext (f :: * -> *) a = (r :: Constraint) | r -> ext where
  GrinSimpleExprExtConstraint constr ext f a = constr (GrinSimpleExprExtType ext f a)


type family (GrinSimpleExprExtType1 (ext :: k) (f:: * -> * )  ) = (r :: * -> *) | r -> ext
--type instance (GrinSimpleExprExtType1 (ext ': r) f = GrinSimpleExprExtType1 (Union ext 


instance {-# OVERLAPPING #-} Functor ( GrinSimpleExprX (ext ': r) f ) where

instance {-# OVERLAPPING #-} Functor (GrinSimpleExprX '[] f) where
  fmap f (UnitX v) = UnitX (fmap f v)
  fmap f (UpdateX n v) = UpdateX n (fmap f v)
  fmap f (CallX n a) = CallX n a


--f :: (Member (GrinSimpleExprExtType1 ext1 f) r, Member (GrinSimpleExprExtType1 ext2 f) r) => 
f = undefined

type family GrinSimpleExprExtConstraint1 (constr :: (* -> *) -> Constraint) ext (f :: * -> *) = (r :: Constraint) | r -> ext where
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

