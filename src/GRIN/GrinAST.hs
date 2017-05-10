{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds, EmptyDataDecls,  TypeInType, TypeOperators, ExistentialQuantification, RankNTypes, DefaultSignatures,
GADTs, DuplicateRecordFields, PatternSynonyms, DeriveTraversable, DeriveGeneric, DeriveDataTypeable, DeriveLift, StandaloneDeriving, DeriveAnyClass #-}
module GrinAST where

import qualified Data.Map.Strict as Map
import Data.Text
import GHC.TypeLits
import GHC.Generics
import Data.Data

import Control.Lens.Plated
import GRIN.GrinIdentifiers
import GRIN.GrinLiteral
import GRIN.GrinVariable
import GRIN.GrinValue
import GRIN.GrinTag
import GRIN.GrinSimpleValue
type Expr = GrinExpr
type Tags = [Tag]
type ModuleName = GrinIdentifier
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
type FieldOffset = Int

data CallConvention
data ForeignEnt
data FFIAnnotation

data Shape = Open | Closed

data GrinExpr f a where
  Seq :: {expr :: SExpr f a, pat :: LPat f a, body :: Expr f a} -> Expr f a
  Case :: {value :: Val f a, alternatives :: Alternatives f a} -> Expr f a
  SimpleExpr :: SExpr f a -> Expr f a
  Fix :: {bnd :: Expr f a} -> Expr f a
  deriving  Typeable

deriving instance (Show a, Show (f (GrinValue f a)), Show (f GrinIdentifier)) => Show (GrinExpr f a)
deriving instance (Data (f a), ValueConstraint f a, Data (f GrinIdentifier))  => Plated (GrinExpr f a) 
  
deriving instance Functor (GrinExpr f)
deriving instance Foldable (GrinExpr f)
deriving instance Traversable (GrinExpr f)
deriving instance (ValueConstraint f a, Data (f a), Data (f GrinIdentifier))  => Data (GrinExpr f a)


instance Traversable f => Applicative (GrinExpr f) where
  pure a = SimpleExpr (Unit (Variable a))
  (SimpleExpr (Unit (Variable f))) <*>  a = f <$> a

instance Traversable f => Monad (GrinExpr f) where
  (SimpleExpr (Unit (Variable a))) >>= f = f a
  
type SExpr = GrinSimpleExpr
type Val = GrinValue
type Name = GrinIdentifier
type VariableName = GrinIdentifier
type FunctionName = GrinIdentifier
data GrinSimpleExpr f a where
  Alloc :: {size :: Val f a} -> SExpr f a -- ^ Ask for a pointer to a node with initial size 'size'.
  Dealloc :: {target :: Pointer a} -> SExpr f a
  Unit :: Traversable f => {value :: Val f a {- FOR FFI ty :: Type-} } -> SExpr f a
  UpdateUnit :: {name :: VariableName, value :: Val f a} -> SExpr f a
  FetchNode :: {name :: Name} -> SExpr f a
  FetchUpdate :: {source :: Name, destination :: Name} -> SExpr f a
  FetchField :: {name :: Name, offset :: FieldOffset, tag ::  Maybe Tag } -> SExpr f a
  Store :: {value :: Val f a} -> SExpr f a -- ^ Equivalent to an 'Alloc' followed by an 'UpdateUnit'.
  Call :: {name :: FunctionName, args :: f GrinIdentifier} -> SExpr f a
{-  FFI :: {name :: Name, callingConvention :: CallConvention, impEnt :: ForeignEnt,
       ffiAnnot :: FFIAnnotation, args :: Arguments} -> SExpr-}
  Eval :: {name :: Name} -> SExpr f a
--  App :: {name :: VariableName, args :: f GrinIdentifier} -> SExpr f a


  deriving  (  Typeable)
deriving instance (Show a, Show (f (GrinValue f a)), Show (f GrinIdentifier)) => Show (GrinSimpleExpr f a)
deriving instance Functor (GrinSimpleExpr f)
deriving instance Foldable (GrinSimpleExpr f)
deriving instance Traversable (GrinSimpleExpr f)
deriving instance (ValueConstraint f a, Data (f GrinIdentifier))  => Data (GrinSimpleExpr f a)

instance Traversable f => Applicative (GrinSimpleExpr f) where
  pure a = Unit (Variable a)
  Unit (Variable f) <*> a = f <$> a
  
type LPat = GrinLambdaPattern
type CPat = GrinConstantPattern

  

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
data GrinAlternative f a where
  Alternative :: {pat :: CPat f , expr :: Expr f a} -> Alternative f a
deriving instance (Show a, Show (f (GrinValue f a)), Show (f GrinIdentifier)) => Show (Alternative f a)


deriving instance Functor (GrinAlternative f)
deriving instance Foldable (GrinAlternative f)
deriving instance Traversable (GrinAlternative f)
deriving instance (ValueConstraint f a, Data (f a), Data (f GrinIdentifier)) => Data (GrinAlternative f a)
deriving instance Typeable (GrinAlternative)

  
data GrinConstantPattern f where
  LiteralPattern ::  GrinLiteral  -> CPat f
  TagPattern ::  Tag -> CPat f
  ConstantNodePattern :: Tag  -> f GrinIdentifier -> CPat f
  deriving (Typeable)

deriving instance (Data (f GrinIdentifier), Typeable f) => Data (GrinConstantPattern f)
deriving instance (Show (f GrinIdentifier)) => Show (GrinConstantPattern f)
