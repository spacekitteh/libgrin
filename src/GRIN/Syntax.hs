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

module GRIN.Syntax where
import Data.Data
import GHC.Generics
import Control.Monad

import Numeric.Natural


data Identifier' a= FunctionName a
                    | ConstructorName a
                    | ArgumentName a
                    | NodeName a
                    | ModuleName a
                    | VariableName a
                    | GlobalName a
                    deriving (Eq, Data, Typeable, Show)

data Literal  =
    LitInteger Integer
  | LitFloat Float
  | LitDouble Double
  | LitChar Char
  | LitBool Bool
  | LitNull
  | LitString String
  | LitLLVMVector Natural Natural VectorType
  deriving (Eq, Ord, Data, Typeable, Show, Generic)
data VectorType = Integral | Floating deriving (Eq, Show, Ord, Data, Typeable)
data SimpleValue a where
  LiteralValue :: Literal -> SimpleValue a
  VariableValue :: Variable a -> SimpleValue a
  deriving (Eq, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

instance Applicative (SimpleValue) where
  pure a = VariableValue (pure a)
  (VariableValue f) <*> (VariableValue a) = VariableValue (f <*> a)
  
data Variable a where
  Var :: a -> Maybe String -> Variable a
  Hole :: Variable a
  deriving (Eq, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

instance Applicative (Variable) where
  pure a = Var a Nothing
  (Var f n) <*> a = f <$> a

type Arity = Natural

data Tag where
  Constructor :: Identifier -> Maybe Arity -> Tag
  FunctionCall :: Identifier -> Maybe Arity -> Tag
  PartialApplication :: Identifier -> Maybe Arity -> Tag
  WhoCaresTag :: Maybe Arity -> Tag -- Distinct from "Tagless" and "Variable" because we don't care what it is
  RecTag :: Tag
  deriving (Eq, Show, Data, Typeable, Generic)

data NodeType a where
  KnownTag :: Tag -> NodeType a
  Tagless :: NodeType a
  VariableTag :: Variable a -> NodeType a
  deriving (Eq, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

data Node a where
  Node :: NodeType a -> [Value a] -> Node a
  deriving (Eq, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

type Pointer a = Variable a

type Identifier = Identifier' String
type VariableName = Identifier
type FunctionName = Identifier
type Label = Identifier


type FieldOffset = Natural

data StackOrHeap = Stack | Heap deriving (Data, Show, Eq)


data Value a where
  SimpleValue :: SimpleValue a -> Value a
  EmptyValue :: Value a
  PlainTag :: Tag -> Value a
  NodeValue :: Node a -> Value a
  PointerValue :: Pointer a -> Value a
  deriving (Eq, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

instance Applicative (Value) where
  pure a = SimpleValue (pure a)
  (SimpleValue f) <*> (SimpleValue a) = SimpleValue (f <*> a)
  
data SimpleExpression a where
  Unit :: Value a -> SimpleExpression a
  Update :: VariableName -> Value a -> SimpleExpression a
  Call :: FunctionName -> [Identifier] -> SimpleExpression a
  NestedExpression :: Expression a -> SimpleExpression a
  -- High level
  Store :: Value a -> SimpleExpression a
  Eval :: Variable a -> SimpleExpression a
  Apply :: Variable a -> [Value a] -> SimpleExpression a
  FetchNode :: Variable a -> SimpleExpression a
  -- Low level
  Alloc :: Natural -> Maybe StackOrHeap -> SimpleExpression a
  Free :: Pointer a -> SimpleExpression a
  FetchUpdate :: {source :: VariableName, target :: VariableName} -> SimpleExpression a
  FetchField :: VariableName -> FieldOffset -> Maybe Tag -> SimpleExpression a
  
  deriving (Eq, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)


instance Applicative (SimpleExpression) where
  pure a = Unit (pure a)
  Unit (SimpleValue (VariableValue (Var f n))) <*> a = f <$> a

pattern EVar a <- SimpleExpr(Unit(SimpleValue(VariableValue(Var a _))))
data Expression a where
  Seq :: SimpleExpression a -> LambdaPattern a -> Expression a -> Expression a
  Case :: Value a -> Alternatives a -> Expression a
  SimpleExpr :: SimpleExpression a -> Expression a
  Fix :: LambdaPattern a -> Expression a -> Maybe (Value a) -> Expression a
  Jump :: Label -> Expression a
  deriving (Eq, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)



instance Applicative (Expression) where
  pure a = SimpleExpr (pure a)
  (<*>) = ap

instance Monad (Expression) where
  (EVar a) >>= f = f a
  



data LambdaPattern a where
  ValuePattern :: Value a -> LambdaPattern a
  deriving (Eq, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

type Alternatives a = [CaseAlternative a]
data CaseAlternative a where
  Alternative :: ConstantPattern a -> Expression a -> CaseAlternative a
  deriving (Eq, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

data ConstantPattern a where
  LiteralPattern :: Literal -> ConstantPattern a
  TagPattern :: Tag -> ConstantPattern a
  ConstantNodePattern :: Tag -> [Variable a] -> ConstantPattern a
  deriving (Eq, Show, Data, Typeable, Functor, Foldable, Traversable, Generic)

