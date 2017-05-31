{-# LANGUAGE DeriveFoldable, DeriveTraversable, UndecidableSuperClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators, StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- GHC >=7.10 deprecated OverlappingInstances in favour of instance by instance
-- annotation using OVERLAPPABLE and OVERLAPPING pragmas.
#ifdef DEPRECATED_LANGUAGE_OVERLAPPING_INSTANCES
#define PRAGMA_OVERLAPPABLE {-# OVERLAPPABLE #-}
#else
{-# LANGUAGE OverlappingInstances #-}
#define PRAGMA_OVERLAPPABLE
#endif

-- |
-- Module:       Data.OpenUnion.Internal
-- Description:  Open unions (type-indexed co-products) for extensible effects.
--
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.
-- License:      BSD3
-- Maintainer:   ixcom-core@ixperta.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- These are internal definitions and should be used with caution. There are no
-- guarantees that the API of this module will be preserved between minor
-- versions of this package.
--
-- Open unions (type-indexed co-products, i.e. type-indexed sums) for
-- extensible effects All operations are constant-time.
--
-- Based on
-- <http://okmij.org/ftp/Haskell/extensible/OpenUnion51.hs OpenUnion51.hs>.
--
-- Type-list @r :: [* -> *]@ of open union components is a small Universe.
-- Therefore, we can use a @Typeable@-like evidence in that universe. In our
-- case a simple index of an element in the type-list is sufficient
-- substitution for @Typeable@.
module Union where
import Data.Maybe
import Data.Foldable
import Data.Traversable
import Data.Bool (otherwise)
import Data.Either (Either(Left, Right))
import Data.Eq ((==))
import Data.Function (($), fix)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Word (Word)
import Data.Functor
import Control.Lens.Plated
import Data.Coerce
import Control.Monad
import Data.Constraint
import Data.Constraint.Lifting
import Data.Proxy
import GHC.TypeLits
import Data.Monoid
import Data.Kind
import Data.Data
import Data.Functor.Identity
import Unsafe.Coerce (unsafeCoerce)


-- | Open union is a strong sum (existential with an evidence).
data Union (constr :: (* -> *) -> Constraint) (r :: [* -> *]) (a :: *)  where
    Union :: constr t => {-# UNPACK #-} !Integer -> t a -> Union constr r a
    deriving (Typeable)


type role Union nominal representational nominal
-- | Takes a request of type @t :: * -> *@, and injects it into the 'Union'.
--
-- Summand is assigning a specified 'Word' value, which is a position in the
-- type-list @(t ': r) :: * -> *@.
--
-- __This function is unsafe.__
--
-- /O(1)/
unsafeInj :: constr t => Integer -> t a -> Union constr r a
unsafeInj = Union
{-# INLINE unsafeInj #-}

-- | Project a value of type @'Union' (t ': r) :: * -> *@ into a possible
-- summand of the type @t :: * -> *@. 'Nothing' means that @t :: * -> *@ is not
-- the value stored in the @'Union' (t ': r) :: * -> *@.
--
-- It is assumed that summand is stored in the 'Union' when the 'Word' value is
-- the same value as is stored in the 'Union'.
--
-- __This function is unsafe.__
--
-- /O(1)/
unsafePrj :: constr t => Integer -> Union constr r a -> Maybe (t a)
unsafePrj n (Union n' x)
  | n == n'   = Just (unsafeCoerce x)
  | otherwise = Nothing
{-# INLINE unsafePrj #-}

-- | Represents position of element @t :: * -> *@ in a type list
-- @r :: [* -> *]@.
newtype P t r = P {unP :: Integer}

-- | Find an index of an element @t :: * -> *@ in a type list @r :: [* -> *]@.
-- The element must exist.
--
-- This is essentially a compile-time computation without run-time overhead.
class FindElem (t :: * -> *) (r :: [* -> *]) where
    -- | Position of the element @t :: * -> *@ in a type list @r :: [* -> *]@.
    --
    -- Position is computed during compilation, i.e. there is no run-time
    -- overhead.
    --
    -- /O(1)/
    elemNo :: P t r

-- | Base case; element is at the current position in the list.
instance FindElem t (t ': r) where
    elemNo = P 0

-- | Recursion; element is not at the current position, but is somewhere in the
-- list.
instance PRAGMA_OVERLAPPABLE FindElem t r => FindElem t (t' ': r) where
    elemNo = P $ 1 + unP (elemNo :: P t r)



-- | This type class is used for two following purposes:
--
-- * As a @Constraint@ it guarantees that @t :: * -> *@ is a member of a
--   type-list @r :: [* -> *]@.
--
-- * Provides a way how to inject\/project @t :: * -> *@ into\/from a 'Union',
--   respectively.
--
-- Following law has to hold:
--
-- @
-- 'prj' . 'inj' === 'Just'
-- @
class (FindElem t r) => Member (t :: * -> *) r where
    -- | Takes a request of type @t :: * -> *@, and injects it into the
    -- 'Union'.
    --
    -- /O(1)/
    inj :: constr t => t a -> Union constr r a

    -- | Project a value of type @'Union' (t ': r) :: * -> *@ into a possible
    -- summand of the type @t :: * -> *@. 'Nothing' means that @t :: * -> *@ is
    -- not the value stored in the @'Union' (t ': r) :: * -> *@.
    --
    -- /O(1)/
    prj :: constr t => Union constr r a -> Maybe (t a)

instance FindElem t r => Member t r where
    inj :: constr t => t a -> Union constr r a
    inj = unsafeInj $ unP (elemNo :: P t r)
    {-# INLINE inj #-}

    prj :: constr t => Union constr r a -> Maybe (t a)
    prj = unsafePrj $ unP (elemNo :: P t r)
    {-# INLINE prj #-}

-- | Orthogonal decomposition of a @'Union' (t ': r) :: * -> *@. 'Right' value
-- is returned if the @'Union' (t ': r) :: * -> *@ contains @t :: * -> *@, and
-- 'Left' when it doesn't. Notice that 'Left' value contains
-- @Union r :: * -> *@, i.e. it can not contain @t :: * -> *@.
--
-- /O(1)/
decomp :: constr t => Union constr (t ': r) a -> Either (Union constr r a) (t a)
decomp (Union 0 a) = Right $ unsafeCoerce a
decomp (Union n a) = Left  $ Union (n - 1) a
{-# INLINE [2] decomp #-}



-- | Specialized version of 'decomp' for efficiency.
--
-- /O(1)/
--
-- TODO: Check that it actually adds on efficiency.
decomp0 :: constr t => Union constr '[t] a -> Either (Union constr '[] a) (t a)
decomp0 (Union _ a) = Right $ unsafeCoerce a
{-# INLINE decomp0 #-}
{-# RULES "decomp/singleton"  decomp = decomp0 #-}

-- | Specialised version of 'prj'\/'decomp' that works on an
-- @'Union' '[t] :: * -> *@ which contains only one specific summand. Hence the
-- absence of 'Maybe', and 'Either'.
--
-- /O(1)/
extract :: constr t => Union constr '[t] a -> t a
extract (Union _ a) = unsafeCoerce a
{-# INLINE extract #-}

-- | Inject whole @'Union' r@ into a weaker @'Union' (any ': r)@ that has one
-- more summand.
--
-- /O(1)/
weaken :: Union constr r a -> Union constr (any ': r) a
weaken (Union n a) = Union (n + 1) a
{-# INLINE weaken #-}


class (a k, b k) =>  (a :&&: b) k where

instance (a k, b k) => (a :&&: b) k where


instance Class (a k, b k)  ((a :&&: b) k) where cls = Sub Dict
instance (a k, b k) :=> ((a :&&: b) k) where ins = Sub Dict

type family Constrain theConstraint constr h tail where
  Constrain theConstraint constr h tail= (constr h, theConstraint h, theConstraint (Union constr tail))


type family Constrain0 theConstraint constr h tail a where
  Constrain0 theConstraint constr h tail a = (constr h, theConstraint (h a), theConstraint (Union constr tail a))

instance (constr k, Functor k) => Functor (Union constr '[k]) where
  fmap f u = inj (fmap f (extract u))
instance  (Constrain Functor constr k rest) => Functor (Union constr (k ': rest)) where
  fmap f u = case decomp u of
               Right t -> (inj (fmap f t))
               Left rest -> weaken (fmap f rest)

instance (Constrain0 Show constr k rest a) => Show (Union constr (k ': rest) a) where
  show u = case decomp u of
             Right t -> show t
             Left t -> show t
instance (Show (k a), constr k) => Show (Union constr '[k] a) where
  show u = show (extract u)

instance (Constrain Foldable constr h tail) => Foldable (Union constr (h ': tail)) where
  foldMap f u = case decomp u of
                  Right t -> foldMap f t
                  Left rest -> foldMap f rest

instance (constr t, Foldable t) => Foldable (Union constr '[t]) where
  foldMap f u = foldMap f (extract u)

instance forall constr h tail . (Constrain Traversable constr h tail) => Traversable (Union constr (h ': tail)) where
  traverse f u = case decomp u of
                   Right t ->  fmap inj (traverse f t)
                   Left t -> fmap weaken $ traverse f t

instance (constr t, Traversable t) => Traversable (Union constr '[t]) where
  traverse f u = fmap inj $  traverse f (extract u)


{-unionConstructor :: Constr
unionConstructor = mkConstr unionDataType "Union" [] Prefix

unionDataType :: DataType
unionDataType = mkDataType "Union" [unionConstructor]
instance (Constrain0 Data constr h tail a, Typeable constr, Typeable h, Typeable tail, Typeable a) => Data (Union constr (h ': tail) a) where
  dataTypeOf _ = unionDataType
  toConstr _ = unionConstructor
  gunfold ::  (forall b r . Data b => c (b -> r) -> c r) -> (forall r. r -> c r) -> Constr -> c (Union constr (h ': tail) a)
  gunfold k z _ =  k (k (z (Union  )))

-}
-- USE PLATED
                                                   
{-instance (Member Identity r, r ~ (h ': tail), constr Identity, Constrain Monad constr h tail, Monad (Union constr (h ': tail))) => Applicative (Union constr (h ': tail)) where
  pure a = inj (Identity a)
  (<*>) = ap



instance (constr k, Applicative k) => Applicative (Union constr '[k]) where
  pure :: forall constr k a . (constr k, Applicative k) => a -> Union constr '[k] a
  pure a = let t  = ((pure @ k a) :: k a) in inj t
  f <*> a = inj ((extract f) <*> (extract a))

instance (Constrain Traversable constr h tail, Constrain Monad constr h tail, r ~ (h ': tail), constr Identity, Member Identity r) => Monad (Union constr (h ': tail)) where
  (>>=) :: Union constr (h ': tail) a -> (a -> Union constr (h ': tail) b) -> Union constr (h ': tail) b
  a''@(Union 0 t) >>= f = fold $ fmap f a where
    a :: h a
    a = unsafeCoerce t

f :: Union (Functor :&&: Foldable :&&: Monad :&&: Applicative) '[Identity, Maybe, []] (Integer -> Integer)
f = pure inc
z = f <*> a
-}
a :: Union (Functor :&&: Foldable :&&: Monad :&&: Applicative) '[Identity, Maybe,[]] Integer
a = inj [5]

inc a = a + 1
foo = fmap inc a
bar = [a, inj (Just 6)]
  

