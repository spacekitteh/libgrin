{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds, EmptyDataDecls,  TypeInType, TypeOperators, ExistentialQuantification, RankNTypes, DefaultSignatures,
GADTs, DuplicateRecordFields, PatternSynonyms, DeriveTraversable, DeriveGeneric, DeriveDataTypeable, DeriveLift, StandaloneDeriving, DeriveAnyClass #-}
module GrinAST where

import qualified Data.Map.Strict as Map
import Data.Text
--import GHC.TypeLits
import GHC.Generics
import Data.Data
import Data.Kind

import Control.Lens.Plated
import GRIN.GrinIdentifiers
import GRIN.GrinLiteral
import GRIN.GrinVariable
import GRIN.GrinValue
import GRIN.GrinTag
import GRIN.GrinCase
import GRIN.GrinSimpleValue
import GRIN.HighLevelGrin
type Expr = GrinExpr
type Tags = [Tag]

data GrinModule where
  Module :: {name :: ModuleName, globalVars :: GlobalVars, bindings :: Bindings,
          tagInfo :: Map.Map Name Tags} -> GrinModule

type GlobalVars = forall a f.  [GlobalVar f a]
type GlobalName = GrinIdentifier
data GlobalVar f a where
  Global :: {name :: GlobalName, value :: Val f a} -> GlobalVar f a

type Bindings = [Binding]
type ArgumentNames = [GrinIdentifier]
data BindingAnnotation
data Binding where
  Bind :: {name :: Name, bindingAnnotation :: BindingAnnotation, argNames :: ArgumentNames, expr :: forall a f . Expr f a} -> Binding
  Arity :: {name :: Name, arity :: Arity} -> Binding
  Rec :: {bindings :: Bindings} -> Binding


type Alternatives f a = [Alternative f a]
data Type 


--data CallConvention
--data ForeignEnt
--data FFIAnnotation

data Shape = Open | Closed

data GrinExpr f a where
  Seq :: {expr :: GrinSimpleExprX ext f a, pat :: LPat f a, body :: Expr f a} -> Expr f a
  Case :: {value :: Val f a, alternatives :: Alternatives f (GrinExpr f a)  } -> Expr f a
  Phi :: {value :: Val f a, alternatives :: GrinValue f a} -> Expr f a
  SimpleExpr :: GrinSimpleExprX ext f a -> Expr f a
  Fix :: {bnd :: Expr f a} -> Expr f a
  deriving  Typeable

deriving instance (Show a, Show (f (GrinValue f a)), Show (f GrinIdentifier)) => Show (GrinExpr f a)
deriving instance (Data (f a), ValueConstraint f a, Data (f GrinIdentifier))  => Plated (GrinExpr f a) 
  
deriving instance Functor (GrinExpr f)
deriving instance Foldable (GrinExpr f)
deriving instance Traversable (GrinExpr f)
deriving instance (ValueConstraint f a, Data (f a), Data (f GrinIdentifier))  => Data (GrinExpr f a)


instance Traversable f => Applicative (GrinExpr f) where
  pure a = SimpleExpr (Unit (Variable a Nothing))
  (SimpleExpr (UnitX (Variable f _))) <*>  a = f <$> a

instance Traversable f => Monad (GrinExpr f) where
  (SimpleExpr (UnitX (Variable a _))) >>= f = f a
  



data GrinSimpleExprX ext f a where
  UnitX :: Traversable f => {value :: Val f a } -> GrinSimpleExprX ext f a -- ^ Returns 'value'.
  UpdateUnitX :: {name :: VariableName, value :: Val f a} -> GrinSimpleExprX ext f a -- ^ Updates the node pointed to by 'name' with 'value', and returns 'value'.
  CallX :: {name :: FunctionName, args :: f GrinIdentifier} -> GrinSimpleExprX ext f a -- ^ Calls a function 'name' with arguments 'args'.
  GrinSimpleExprExt :: GrinSimpleExprExtType ext f a -> GrinSimpleExprX ext f a
  
{-  FFI :: {name :: Name, callingConvention :: CallConvention, impEnt :: ForeignEnt,
       ffiAnnot :: FFIAnnotation, args :: Arguments} -> SExpr-}
  deriving Typeable
type family  GrinSimpleExprExtType ext (f:: * -> *) a where
  GrinSimpleExprExtType ext f a = GrinSimpleExprExtType1 ext f a

type family (GrinSimpleExprExtType1 ext (f:: * -> * )  ) :: * -> *
type family GrinSimpleExprExtConstraint1 ext (f :: * -> *) ::  Constraint where
  GrinSimpleExprExtConstraint1 ext f = GrinSimpleExprExtType1 ext f

type instance GrinSimpleExprExtType1 HighLevelGrin f  = HighLevelSimpleExpression HighLevelGrin f


deriving instance (Show a, Show (f (GrinValue f a)), Show (f GrinIdentifier)) => Show (GrinSimpleExprX ext f a)
deriving instance (Functor (GrinSimpleExprExtConstraint1 ext f )) => Functor (GrinSimpleExprX ext f)
--deriving instance Foldable (GrinSimpleExprX ext f)
--deriving instance Traversable (GrinSimpleExprX ext f)
--deriving instance (ValueConstraint f a, Data (f GrinIdentifier))  => Data (GrinSimpleExprX ext f a)

instance Traversable f => Applicative (GrinSimpleExprX ext f) where
  pure a = UnitX (Variable a Nothing)
  UnitX (Variable f _) <*> a = f <$> a
  
type LPat = GrinLambdaPattern

  
data GrinLambdaPattern f a where
  ValuePattern :: GrinValue f a -> LPat f a
  deriving (Functor, Foldable, Traversable, Typeable)
deriving instance (Show a, Show (f (GrinValue f a))) => Show (GrinLambdaPattern f a)
deriving instance ValueConstraint f a  => Data (GrinLambdaPattern f a)
pattern EmptyPattern = ValuePattern (EmptyValue)
pattern VariablePattern name = ValuePattern (SimpleValue (VarValue name))
pattern PlainTagPattern tag = ValuePattern(PlainTag tag)
pattern KnownTagNodePattern tag fields = ValuePattern (NodeValue ( Node (KnownTag tag) fields))
pattern VariableTagNodePattern tagName fields = ValuePattern (NodeValue ( Node (VariableTag tagName) fields))






type Alternative = GrinAlternative
