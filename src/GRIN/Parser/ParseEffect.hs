{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators, TypeApplications, GADTs, DataKinds, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds, EmptyDataDecls, TypeOperators, ExistentialQuantification, RankNTypes, DefaultSignatures,ScopedTypeVariables,
GADTs, DuplicateRecordFields, PatternSynonyms, DeriveTraversable, DeriveGeneric, DeriveDataTypeable, DeriveLift, StandaloneDeriving, DeriveAnyClass, MultiParamTypeClasses, TypeApplications #-}
-}
module GRIN.Parser.ParseEffect where

import Control.Monad.Freer
import Control.Monad.Freer.Internal (NonDet, Eff(..), decomp, qApp, qComp )
import Control.Monad.Freer.Fresh
import qualified Control.Monad.Freer.State as S
import Text.Megaparsec.Prim hiding (runParser)
import Data.Set as E
import Data.Kind
import Data.Semigroup
import GHC.Generics
import Data.Data
import Text.Megaparsec.Error (ErrorItem, ParseError(..), ErrorComponent)
import qualified Data.List.NonEmpty as NE
newtype Hints t = Hints [Set (ErrorItem t)] deriving (Semigroup, Monoid)

{-

toHints :: ParseError t e -> Hints t
toHints err = Hints hints
  where hints = if E.null msgs then [] else [msgs]
        msgs  = errorExpected err
{-# INLINE toHints #-}

withHints :: Ord (Token s)
  => Hints (Token s)   -- ^ Hints to use
  -> (ParseError (Token s) e -> State s -> m b) -- ^ Continuation to influence
  -> ParseError (Token s) e -- ^ First argument of resulting continuation
  -> State s           -- ^ Second argument of resulting continuation
  -> m b
withHints (Hints ps') c e@(ParseError pos us ps xs) =
  if E.null us && E.null ps && not (E.null xs)
    then c e
    else c (ParseError pos us (E.unions (ps : ps')) xs)
{-# INLINE withHints #-}


accHints
  :: Hints t           -- ^ 'Hints' to add
  -> (a -> State s -> Hints t -> m b) -- ^ An “OK” continuation to alter
  -> a                 -- ^ First argument of resulting continuation
  -> State s           -- ^ Second argument of resulting continuation
  -> Hints t           -- ^ Third argument of resulting continuation
  -> m b
accHints hs1 c x s hs2 = c x s (hs1 <> hs2)
{-# INLINE accHints #-}

refreshLastHint :: Hints t -> Maybe (ErrorItem t) -> Hints t
refreshLastHint (Hints [])     _        = Hints []
refreshLastHint (Hints (_:xs)) Nothing  = Hints xs
refreshLastHint (Hints (_:xs)) (Just m) = Hints (E.singleton m : xs)
{-# INLINE refreshLastHint #-}

data ParsecEffect e s a where
  ConsumedOK :: a -> State s -> Hints (Token s) -> ParsecEffect e s a
  ConsumedError :: ParseError (Token s) e -> State s -> ParsecEffect e s (ParseError (Token s) e)
  EmptyOK :: a -> State s -> Hints (Token s) -> ParsecEffect e s a
  EmptyError :: ParseError (Token s) e -> State s -> ParsecEffect e s (ParseError (Token s) e)

cok :: forall e s a b effs rest . (ErrorComponent e, Stream s, Member (ParsecEffect e s) effs) => a -> State s -> Hints (Token s) -> Eff effs a
cok a s h = send @(ParsecEffect e s) (ConsumedOK  a s h)
cerr :: forall e s effs. (ErrorComponent e, Stream s, Member (ParsecEffect e s) effs) => ParseError (Token s) e -> State s -> Eff effs  (ParseError (Token s) e)
cerr e s= send @(ParsecEffect e s)  (ConsumedError e s)
eok :: forall e s a b effs rest . (ErrorComponent e, Stream s, Member (ParsecEffect e s) effs) => a -> State s -> Hints (Token s) -> Eff effs a
eok a s h = send @(ParsecEffect e s) (EmptyOK a s h)
eerr :: forall e s  effs. (ErrorComponent e, Stream s, Member (ParsecEffect e s) effs) => ParseError (Token s) e -> State s -> Eff effs (ParseError (Token s) e)
eerr e s = send @(ParsecEffect e s) (EmptyError e s)


failure :: forall e s effs. (ErrorComponent e, Stream s, Member (ParsecEffect e s) effs, Member (S.State (State s)) effs) => Set (ErrorItem (Token s)) -> Set (ErrorItem (Token s)) -> Set e -> Eff effs (ParseError (Token s) e)
failure us ps xs = do
  s@(State _ pos _ _) <- S.get @(State s)
  eerr (ParseError pos us ps xs) s

label :: forall e s effs a . (ErrorComponent e, Stream s, Member (ParsecEffect e s) effs, Member (S.State (State s)) effs) => String -> Eff effs a -> Eff effs a
label l p = do
  s <- S.get @(State s)
  let el = Label <$> NE.nonEmpty l
      cl = Label . (NE.fromList "the rest of "<>) <$> NE.nonEmpty l
      
  error "unimplemented"


-}


data Parser e s a where
  Failure :: Set (ErrorItem (Token s)) -> Set (ErrorItem (Token s)) -> Set e -> Parser e s a
  Label :: Member (Parser e s) effs => String -> Eff effs a -> Parser e s a
  Hidden :: Member (Parser e s) effs => Eff effs a -> Parser e s a
  Try :: Member (Parser e s) effs => Eff effs a -> Parser e s a
  LookAhead :: Member (Parser e s) effs => Eff effs a -> Parser e s a
  NotFollowedBy :: Member (Parser e s) effs => Eff effs a -> Parser e s ()
  WithRecovery :: Member (Parser e s) effs => (ParseError (Token s) e -> Eff effs a) -> Eff effs a -> Parser e s a
  Observing :: Member (Parser e s) effs => Eff effs a -> Parser e s (Either (ParseError (Token s) e) a)
  EOF :: Parser e s ()
  Token :: (Token s -> Either (Set (ErrorItem (Token s)), Set (ErrorItem (Token s)), Set e) a) -> Maybe (Token s) -> Parser e s a
  Tokens :: (Token s -> Token s -> Bool) -> [Token s] -> Parser e s [Token s]
  GetParserState :: Parser e s (State s)
  UpdateParserState :: (State s -> State s) -> Parser e s ()

class MPC e s (effs :: [* -> *]) | effs -> e s where
  type C e s effs :: Constraint
  
instance MPC e s ((Parser e s) : rest) where
  type C e s ((Parser e s) : rest)  = Member (Parser e s) ((Parser e s) : rest)

instance Member (Parser e s) rest => MPC e s (h : rest) where
  type C e s (h : rest) = Member (Parser e s) (h : rest)
  

  

instance  (ErrorComponent e, Stream s, Member (Parser e s) effs, Member NonDet effs) => MonadParsec e s (Eff effs) where
  failure a b c = send @(Parser e s) (Failure a b c)
  label a b = send @(Parser e s) (Label :: String -> Eff effs a -> Parser e s a) a b
  hidden a = send @(Parser e s) (Hidden a)
  try a = send @(Parser e s) (Try a)
  lookAhead a = send @(Parser e s) (LookAhead a)
  notFollowedBy a = send @(Parser e s) (NotFollowedBy a)
  withRecovery a b = send @(Parser e s) (WithRecovery a b)
  observing a = send @(Parser e s) (Observing a)
  eof = send @(Parser e s) EOF
  token a b = send @(Parser e s) (Token a b)
  tokens a b = send @(Parser e s) (Tokens a b)
  getParserState = send @(Parser e s) GetParserState
  updateParserState a = send @(Parser e s) (UpdateParserState a)


runParser :: forall e s effs a . Eff (Parser e s ': effs) a -> State s -> Eff effs (State s, Either (ParseError (Token s) e) a)
runParser (Val x) s = return (s, Right x)
runParser (E u q) s= case decomp u of
  Right (Failure a b c) -> _ a b c --  runParser (qApp q (_ a b c)) s
