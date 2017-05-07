{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds, EmptyDataDecls,  TypeInType, TypeOperators, ExistentialQuantification, RankNTypes, DefaultSignatures,
GADTs, DuplicateRecordFields, PatternSynonyms, DeriveTraversable, DeriveGeneric, DeriveDataTypeable, DeriveLift, StandaloneDeriving #-}
module GrinAST where

import qualified Data.Map.Strict as Map
import Data.Text
import GHC.TypeLits
import GHC.Generics
import Data.Data
import Language.Haskell.TH.Syntax (Lift)
import Control.Lens.Plated
import GRIN.GrinIdentifiers
import GRIN.GrinLiteral
import GRIN.GrinVariable
import GRIN.GrinValue
import GRIN.GrinTag
import GRIN.GrinSimpleValue
type Expr = GrinExpr
type Tags = [Tag]
data GrinModule where
  Module :: {name :: ModuleName, globalVars :: GlobalVars, bindings :: Bindings,
          tagInfo :: Map.Map Name Tags} -> GrinModule

type GlobalVars = forall a f.  [GlobalVar f a]
data GlobalVar f a where
  Global :: {name :: GlobalName, value :: Val f a} -> GlobalVar f a

type Bindings = [Binding]
data BindingAnnotation
data Binding where
  Bind :: {name :: Name, bindingAnnotation :: BindingAnnotation, argNames :: ArgumentNames, expr :: forall a f . Expr f a} -> Binding
  Arity :: {name :: Name, arity :: Arity} -> Binding
  Rec :: {bindings :: Bindings} -> Binding


type Alternatives f a = [Alternative f a]
data Type 
type FieldOffset = Int
data Argument = Argument  deriving (Data, Typeable, Eq)
type Arguments = [Argument]
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


instance (Typeable f, Traversable f, Typeable a, Data a, Data (f a), Data (f (GrinValue f a))) => Plated (GrinExpr f a) where
  
deriving instance Functor (GrinExpr f)
deriving instance Foldable (GrinExpr f)
deriving instance Traversable (GrinExpr f)
deriving instance (Typeable f, Traversable f, Typeable a, Data a, Data (f a), Data (f (GrinValue f a))) => Data (GrinExpr f a)


instance Traversable f => Applicative (GrinExpr f) where
  pure a = SimpleExpr (Unit (Variable a))
  (SimpleExpr (Unit (Variable f))) <*>  a = f <$> a

instance Traversable f => Monad (GrinExpr f) where
  (SimpleExpr (Unit (Variable a))) >>= f = f a
  
type SExpr = GrinSimpleExpr
data GrinSimpleExpr f a where
  Alloc :: {size :: Val f a} -> SExpr f a
  Dealloc :: {target :: Val f a} -> SExpr f a
  Unit :: Traversable f => {value :: Val f a {- FOR FFI ty :: Type-} } -> SExpr f a
  UpdateUnit :: {name :: VariableName, value :: Val f a} -> SExpr f a
  FetchNode :: {name :: Name} -> SExpr f a
  FetchUpdate :: {source :: Name, destination :: Name} -> SExpr f a
  FetchField :: {name :: Name, offset :: FieldOffset, tag ::  Maybe Tag } -> SExpr f a
  Store :: {value :: Val f a} -> SExpr f a
  Call :: {name :: FunctionName, args :: Arguments} -> SExpr f a
{-  FFI :: {name :: Name, callingConvention :: CallConvention, impEnt :: ForeignEnt,
       ffiAnnot :: FFIAnnotation, args :: Arguments} -> SExpr-}
  Eval :: {name :: Name} -> SExpr f a
  App :: {name :: VariableName, args :: Arguments} -> SExpr f a
{-  Throw {name :: Name, args :: Arguments} :: Expr
  Catch { body :: Expr, arg :: Name, handler :: Expr} :: Expr  -}

  deriving  (  Typeable)
deriving instance Functor (GrinSimpleExpr f)
deriving instance Foldable (GrinSimpleExpr f)
deriving instance Traversable (GrinSimpleExpr f)
deriving instance (Typeable f, Traversable f, Typeable a, Data a, Data (f a), Data (f (GrinValue f a))) => Data (GrinSimpleExpr f a)

instance Traversable f => Applicative (GrinSimpleExpr f) where
  pure a = Unit (Variable a)
  Unit (Variable f) <*> a = f <$> a
  
type LPat = GrinLambdaPattern
type CPat = GrinConstantPattern

  

data GrinLambdaPattern f a where
  ValuePattern :: Val f a -> LPat f a
  --todo: add ignore pattern
  deriving (Functor, Foldable, Traversable, Typeable)
deriving instance (Typeable f, Traversable f, Typeable a, Data a, Data (f a), Data (f (GrinValue f a))) => Data (GrinLambdaPattern f a)
pattern EmptyPattern = ValuePattern (EmptyValue)
pattern VariablePattern name = ValuePattern (SimpleValue (VarValue name))
pattern PlainTagPattern tag = ValuePattern(PlainTag tag)
pattern KnownTagNodePattern tag = ValuePattern(NodeValue tag)
pattern VariableTagNodePattern tag fields = ValuePattern (NodeValue ( Node (KnownTag tag) fields))






type Alternative = GrinAlternative
data GrinAlternative f a where
  Alternative :: {pat :: CPat , expr :: Expr f a} -> Alternative f a

deriving instance Functor (GrinAlternative f)
deriving instance Foldable (GrinAlternative f)
deriving instance Traversable (GrinAlternative f)
deriving instance (Typeable f, Traversable f, Typeable a, Data a, Data (f a), Data (f (GrinValue f a))) => Data (GrinAlternative f a)
deriving instance Typeable (GrinAlternative)

  
data GrinConstantPattern where
  LiteralPattern ::  GrinLiteral  -> CPat 
  TagPattern ::  Tag -> CPat 
  ConstantNodePattern :: Tag  -> VariableNames -> CPat 
  deriving (Data, Typeable)
