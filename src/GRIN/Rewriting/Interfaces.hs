module GRIN.Rewriting.Interfaces where

import Control.Monad.Freer

data Heap -- Abstracts both store/storeless models into a hybrid heap model.

data Trace -- Models the control-flow graph. Includes commands for splitting + merging program traces based on trace partitioning. Generalise flow sensitive + context sensitive models.

data Relational -- Generalised polyhedral domain, etc

data Constant -- Simple constant propagation/folding.

data VariableLiveness -- Liveness of variables.

data AbstractGarbageCollection

data PointerAnalysis -- Points-to analysis, shape analysis, and other such things.

data RegionAnalysis -- Tracking resources, memory use-def analysis, etc. Related? Maybe: https://www.cs.cmu.edu/afs/cs/academic/class/15745-s12/public/lectures/L12-Interval-Analysis-1up.pdf also https://ecommons.cornell.edu/bitstream/handle/1813/5668/TR2004-1968.pdf

data AliasAnalysis

data ConstraintAnalysis -- Introduce generic constraints to be solved by a constraint solver.

data Implication -- Interface for arbitrary reduced cardinal power (or more generally, reduced relative power) construction. Not a domain itself, but a method of combining domains. Useful for, e.g. case splitting. https://pdfs.semanticscholar.org/cd71/a3fab8b5375d93769b290b6ab2b4ebb89bad.pdf

data StronglyConnectedComponents -- Use for information about recursion/loops/cycles in the CFG.


{-
  Each analysis type is an Effect - Is this an implementation of Open Products? https://pdfs.semanticscholar.org/f1af/70db5a42711c2f7bc8f8689e6e7f597b51ab.pdf

  

  Ability to define specific reduction operations between domains, otherwise use generic Nielsen-Oppen decision procedure which is an iterated pairwise reduction.


-}
