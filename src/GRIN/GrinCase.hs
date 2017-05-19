{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}


module GRIN.GrinCase where
import GRIN.GrinIdentifiers
import GRIN.GrinLiteral
import GRIN.GrinValue
import GRIN.GrinTag

import Data.Data


data GrinAlternative f expr  where
  Alternative :: {pat :: GrinConstantPattern f , expr :: expr } -> GrinAlternative f expr
deriving instance (Show expr, Show (f GrinIdentifier)) => Show (GrinAlternative f expr)


deriving instance Functor (GrinAlternative f  )
deriving instance Foldable (GrinAlternative f  )
deriving instance Traversable (GrinAlternative f  )
deriving instance (Data expr, Typeable f, Data (f GrinIdentifier)) =>  Data (GrinAlternative f expr)
deriving instance Typeable (GrinAlternative)

  
data GrinConstantPattern f where
  LiteralPattern ::  GrinLiteral  -> GrinConstantPattern f
  TagPattern ::  Tag -> GrinConstantPattern f
  ConstantNodePattern :: Tag  -> f GrinIdentifier -> GrinConstantPattern f
  deriving (Typeable)

deriving instance (Data (f GrinIdentifier), Typeable f) => Data (GrinConstantPattern f)
deriving instance (Show (f GrinIdentifier)) => Show (GrinConstantPattern f)
