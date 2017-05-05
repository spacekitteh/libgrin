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

type Name = String
type ModuleName = Name
type GlobalName = Name
type NodeName = Name
type ArgumentName = VariableName
type VariableName = Name
type VariableNames = [VariableName]
type ArgumentNames = [ArgumentName]
type ConstructorName = NodeName
type FunctionName = Name

type Arity = Int

type Expr = GrinExpr

data GrinModule where
  Module :: {name :: ModuleName, globalVars :: GlobalVars, bindings :: Bindings,
          tagInfo :: Map.Map Name Tags} -> GrinModule

type GlobalVars = forall a.  [GlobalVar a]
data GlobalVar a where
  Global :: {name :: GlobalName, value :: Val a} -> GlobalVar a

type Bindings = [Binding]
data BindingAnnotation
data Binding where
  Bind :: {name :: Name, bindingAnnotation :: BindingAnnotation, argNames :: ArgumentNames, expr :: forall a. Expr a} -> Binding
  Arity :: {name :: Name, arity :: Arity} -> Binding
  Rec :: {bindings :: Bindings} -> Binding


type Alternatives = [Alternative]
data Type 
type FieldOffset = Int
data Argument
type Arguments = [Argument]
data CallConvention
data ForeignEnt
data FFIAnnotation

data Shape = Open | Closed

data GrinExpr a where
  Seq ::forall a' a. {expr :: SExpr a', pat :: LPat a', body :: Expr a} -> Expr a
  Case :: {value :: Val a, alternatives :: Alternatives} -> Expr a
  SimpleExpr :: SExpr a -> Expr a


  deriving  Typeable
deriving instance Functor GrinExpr
deriving instance Foldable GrinExpr
deriving instance Traversable GrinExpr
--deriving instance Data (GrinExpr a)
--deriving instance Lift (GrinExpr a)

instance Applicative GrinExpr where
  pure a = SimpleExpr (Unit (SimpleValue (VarValue (Var a))))

type SExpr = GrinSimpleExpr
data GrinSimpleExpr a where
  Unit ::{value :: Val a {- FOR FFI ty :: Type-} } -> SExpr a
  UpdateUnit :: {name :: VariableName, value :: Val a} -> SExpr a
  FetchNode :: {name :: Name} -> SExpr a
  FetchUpdate :: {source :: Name, destination :: Name} -> SExpr a
  FetchField :: {name :: Name, offset :: FieldOffset, tag :: forall ty. Maybe (Tag ty)} -> SExpr a
  Store :: {value :: Val a} -> SExpr a
  Call :: {name :: FunctionName, args :: Arguments} -> SExpr a
{-  FFI :: {name :: Name, callingConvention :: CallConvention, impEnt :: ForeignEnt,
       ffiAnnot :: FFIAnnotation, args :: Arguments} -> SExpr-}
  Eval :: {name :: Name} -> SExpr a
  App :: {name :: VariableName, args :: Arguments} -> SExpr a
{-  Throw {name :: Name, args :: Arguments} :: Expr
  Catch { body :: Expr, arg :: Name, handler :: Expr} :: Expr  -}

  deriving (Functor, Foldable, Traversable,  Typeable)
--deriving instance Data (SExpr a)
--deriving instance Lift (SExpr a)
  


type Val = GrinValue
type Vals  = forall k. [ Val k]
data Boxing
data GrinLiteral ty where
  StringLiteral :: Text -> GrinLiteral Text
  IntegerLiteral :: Integer -> GrinLiteral Integer



data GrinNode ty where
  Node :: {tag :: Tag ty, fields :: Vals} -> GrinNode ty
  VariableNode :: {tagName :: VariableName, fields :: Vals} -> GrinNode ty
  
  

data GrinValue (ty::k)  where
  SimpleValue :: SVal ty -> Val ty
  EmptyValue :: Val ty
  PlainTag :: {tag ::  GrinTag ty } -> Val ty
  NodeValue :: GrinNode ty -> Val ty
--  Box :: {boxing :: Boxing, field :: Val} -> Val
instance Functor GrinValue where
  fmap f (SimpleValue t) = SimpleValue (fmap f t)
type SVal = GrinSimpleValue
data GrinSimpleValue (a::k) where
  Literal :: {value :: GrinLiteral ty} -> SVal ty
  VarValue ::  Variable ty -> SVal ty
instance Functor SVal where
  fmap f (VarValue a) = VarValue (fmap f a)
  
type Variable = GrinVariable
data GrinVariable (ty::k) where
  Var :: ty -> Variable ty
  NamedVar :: {name :: VariableName} -> Variable ty
  KnownTag :: {tag :: Tag ty} -> Variable ty
  Ignore :: Variable ty

instance Functor (GrinVariable) where
  fmap f (Var t)  = Var (f t)

instance Applicative (GrinVariable) where
  pure f (Var t) = Var (f t)

deriving instance Foldable GrinVariable
deriving instance Traversable GrinVariable
  
type LPat = GrinLambdaPattern
type CPat = GrinConstantPattern

  

data GrinLambdaPattern a where
  ValuePattern :: Val a -> LPat a
  deriving Functor
pattern EmptyPattern = ValuePattern (EmptyValue)
pattern VariablePattern name = ValuePattern (SimpleValue (VarValue name))
pattern PlainTagPattern tag = ValuePattern(PlainTag tag)
pattern KnownTagNodePattern tag = ValuePattern(NodeValue tag)
pattern VariableTagNodePattern tag fields = ValuePattern (NodeValue ( VariableNode tag fields))




type Tag = GrinTag 
data Tags
data GrinTagAnnotation
data TagDataInfo

data GrinTagType  = ConstructorTag  | FullyAppliedFunctionApplicationTag | PartialApplicationTag | NoTag deriving (Data, Typeable, Generic)

pattern EnumTag name = Constructor name 0

data GrinTag (ty::GrinTagType) where
  Constructor ::  ConstructorName -> Arity -> Tag ConstructorTag
  FunctionCall ::  FunctionName -> Arity -> Tag FullyAppliedFunctionApplicationTag
  PartialApplication :: FunctionName -> Arity -> Tag PartialApplicationTag
--  Application :: { name :: Name } -> Tag
  Unboxed :: Tag NoTag
  Hole :: Tag ty
  RecTag :: Tag ty
  deriving ( Typeable)
--deriving instance Data (GrinTag t)

type Alternative = GrinAlternative
data GrinAlternative where
  Alternative :: {pat :: forall ty. CPat ty, expr :: forall a. Expr a} -> Alternative

  
data GrinConstantPattern (ty)  where
  LiteralPattern ::  GrinLiteral litTy -> CPat NoTag
  TagPattern ::  Tag ty-> CPat ty
  ConstantNodePattern :: Tag ty -> VariableNames -> CPat ty
