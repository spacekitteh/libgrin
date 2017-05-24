{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds , PolyKinds #-}
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
import Control.Applicative
import Data.Functor.Identity
import Data.Maybe (fromJust)
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


--type family  GrinSimpleExprExtType (ext::k) (f:: * -> *) a = (r :: *)   where
--  GrinSimpleExprExtType ext f a = GrinSimpleExprExtType1 ext f a


--type family GrinSimpleExprExtConstraint (constr :: * -> Constraint) ext (f :: * -> *) a = (r :: Constraint)  where
--  GrinSimpleExprExtConstraint constr ext f a = constr (GrinSimpleExprExtType ext f a)


class (Functor (SExprExt ext f), Foldable (SExprExt ext f), Traversable (SExprExt ext f)) =>  SimpleExprExtension ext f a where
  type  SExprExt  ext f  = (r :: (* -> *)) | r -> ext
  functorWitness :: p ext f a -> Dict (Functor (SExprExt ext f))
  functorWitness _ = Dict
  foldableWitness :: p ext f a -> Dict (Foldable (SExprExt ext f))
  foldableWitness _ = Dict
  traversableWitness :: p ext f a -> Dict (Traversable (SExprExt ext f))
  traversableWitness _ = Dict

--type family (GrinSimpleExprExtType1 (ext :: k) (f:: * -> * )  ) = (r :: * -> *) 



type family MapList (l :: [ * -> * ]) f  = (r :: [* -> *] ) | r -> l  where
  MapList '[] f = '[]
  MapList (ext ': '[]) f = '[SExprExt ext f]
  MapList (ext1 ': ext2 ': rest) f = (SExprExt ext1 f) : (SExprExt ext2 f) : MapList rest f

--type instance GrinSimpleExprExtType1 (Union (ext :: [ * -> * ])) f  = Union (MapList ext f)

instance SimpleExprExtension ('[] :: [* -> *]) f a where
  type SExprExt ('[] :: [* -> *]) f = Union ((MapList ('[] :: [* -> *]) f))

instance SimpleExprExtension ((ext ': '[]) :: [* -> *]) f a where
  type SExprExt ((ext ': '[]) :: [* -> *]) f = Union (MapList ((ext ': '[]) :: [* -> *]) f)

instance  SimpleExprExtension ((ext1 ': ext2 ': rest) :: [* -> *]) f a where
  type SExprExt (ext1 ': ext2 ': rest) f = Union (MapList ((ext1 ': ext2 ': rest) :: [* -> *]) f)



                                                              
type family ConstrainedMembers constr l = (r :: Constraint) where
  ConstrainedMembers constr (t ': c) = (constr t, ConstrainedMembers constr c)
  ConstrainedMembers constr '[] = ()



instance {-#OVERLAPPING #-} Functor (Union '[]) where
  fmap f a = error "Absurd"

instance {-#OVERLAPPING #-} (ConstrainedMembers Functor r,  r ~ (h : tail), Functor (Union tail)) => Functor (Union r) where
  fmap f a = case decomp a of
               Right t -> inj (fmap f t)
               Left remainder -> weaken (fmap f remainder)


instance {-#OVERLAPPING#-} Foldable (Union '[]) where
  foldMap _ _ = mempty

instance {-#OVERLAPPING#-} (ConstrainedMembers Foldable r, r ~ (h : tail), Foldable (Union tail)) => Foldable (Union r) where
  foldMap f a = case decomp a of
                  Right t -> foldMap f t
                  Left remainder ->  foldMap f remainder

instance {-#OVERLAPPING#-} Traversable (Union '[]) where
  traverse f t = error "What to do here?!"

instance {-#OVERLAPPING#-} (ConstrainedMembers Traversable r, r ~ (h : tail), Traversable (Union tail)) => Traversable (Union r) where
  traverse (f :: _) t = case decomp t of
                   Right (a:: h a) -> pure _
                   Left remainder -> _ (traverse f remainder)

instance {-#OVERLAPPING#-} Applicative (Union '[]) where
  pure  = error "Absurd!"
  (<*>) = error "Absurd!"

instance {-#OVERLAPPING#-} (Member (Identity) r, ConstrainedMembers Functor tail, ConstrainedMembers Applicative r, r ~ (h : tail), Applicative (Union tail)) => Applicative (Union r) where
  pure t = inj (Identity t)
  f' <*> a' = let a =  fromJust (prj a') in
    case decomp f' of
              Right f -> inj (f <*> ( a))

{-type family GrinSimpleExprExtConstraint1 (constr :: (* -> *) -> Constraint) ext (f :: * -> *) = (r :: Constraint)  where
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


deriving instance forall k (ext::k) f a. (Typeable k, ValueConstraint f a, Data (f GrinIdentifier), GrinSimpleExprExtConstraint Data ext f a, Typeable ext, Traversable (GrinSimpleExprExtType1 ext f))  => Data (GrinSimpleExprX ext f a)

instance (Traversable f, GrinSimpleExprExtConstraint1 Functor ext f) => Applicative (GrinSimpleExprX ext f) where
  pure a = UnitX (Variable a Nothing)
  UnitX (Variable f n) <*> a = (f <$> a)

-}
