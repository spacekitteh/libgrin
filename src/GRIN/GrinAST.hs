{-# LANGUAGE TypeFamilyDependencies #-}
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
import Data.Constraint
import Control.Lens.Plated
import GRIN.GrinIdentifiers
import GRIN.GrinLiteral
import GRIN.GrinVariable
import GRIN.GrinValue
import GRIN.GrinTag
import GRIN.GrinCase
import GRIN.GrinSimpleValue
import GRIN.HighLevelGrin
import GRIN.GrinSimpleExpression
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
  Bind :: {name :: Name, bindingAnnotation :: BindingAnnotation, argNames :: ArgumentNames, expr :: forall a f ext . Expr ext f a} -> Binding
  Arity :: {name :: Name, arity :: Arity} -> Binding
  Rec :: {bindings :: Bindings} -> Binding


type Alternatives f a = [Alternative f a]
data Type 


--data CallConvention
--data ForeignEnt
--data FFIAnnotation

data Shape = Open | Closed

data GrinExpr ext f a where
  Seq :: {expr :: GrinSimpleExprX ext f a, pat :: LPat f a, body :: Expr ext f a} -> Expr ext f a
  Case :: {value :: Val f a, alternatives :: Alternatives f (GrinExpr ext f a)  } -> Expr ext f a
  Phi :: {value :: Val f a, possibleValues :: GrinValue f a} -> Expr ext f a
  SimpleExpr :: GrinSimpleExprX ext f a -> Expr ext f a
  Fix :: {bound :: LPat f a, bnd :: Expr ext f a} -> Expr ext f a
  deriving  Typeable

deriving instance (Show a, Show (f (GrinValue f a)), Show (f GrinIdentifier), Show (GrinSimpleExprExtType ext f a)) => Show (GrinExpr ext f a)
deriving instance (Data (f a), ValueConstraint f a, Data (f GrinIdentifier))  => Plated (GrinExpr ext f a) 
  
deriving instance Functor (GrinExpr ext f)
deriving instance Foldable (GrinExpr ext f)
deriving instance Traversable (GrinExpr ext f)
deriving instance (ValueConstraint f a, Data (f a), Data (f GrinIdentifier))  => Data (GrinExpr ext f a)


instance Traversable f => Applicative (GrinExpr ext f) where
  pure a = SimpleExpr (UnitX (Variable a Nothing))
  (SimpleExpr (UnitX (Variable f _))) <*>  a = f <$> a

instance Traversable f => Monad (GrinExpr ext f) where
  (SimpleExpr (UnitX (Variable a _))) >>= f = f a
  



type instance GrinSimpleExprExtType1 HighLevelGrin f  = HighLevelSimpleExpression HighLevelGrin f

  
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
