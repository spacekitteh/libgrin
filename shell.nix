{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithHoogle (ps: with ps; 
[ ghc-mod
 hspec
 unbound
 ghc-typelits-presburger
  presburger
  equational-reasoning
 bound
 graph-rewriting
 hspec-megaparsec
 hspec-laws
 hspec-expectations-lens
 hspec-checkers
 hspec-smallcheck
 freer-effects
 lens_4_15_2
 megaparsec
 hoopl
 constraints
 QuickCheck
 ansi-wl-pprint 
 criterion
 haskintex
 freer
 text
 plan-applicative
 text-format
 annotated-wl-pprint
 cabal-install
 mainland-pretty
 trifecta
 alex
 zippers
# compdata
 fgl
 doctest
 hslogger 
 happy
 monad-logger
 hlint
 unordered-containers
 ekg
 haskeline
 #ivory 
 #smartcheck
 singletons
 sbv
 pipes
 tasty
 optparse-applicative
 configurator 
 data-category
# llvm-hs-pure_4_1_0_0
# llvm-hs
# llvm-hs-typed
# llvm-hs-pretty

 megaparsec
 idris
 structured-haskell-mode
]);
   
 emacsWithMyPackages = let customEmacsPackages = pkgs.emacsPackagesNg.overrideScope (super: self: {
      emacs = pkgs.emacs.override { withGTK2 = true; withGTK3 = false; }; }); in
      customEmacsPackages.emacsWithPackages         
        (epkgs: 
         (with pkgs.emacsPackagesNg; [ 
            ghc-mod
	    org
            haskell-mode
            structured-haskell-mode
            rainbow-delimiters
            company-ghc
            flycheck-haskell
            org-plus-contrib
            idris-mode
            org-trello            
            auctex
            magit 
           ]) ++ ([pkgs.emacsPackages.proofgeneral_HEAD]));

  llvm = pkgs.llvm_39;
  git = pkgs.git;
  cabal2nix = pkgs.cabal2nix;
  busybox = pkgs.busybox;
  sphinx = pkgs.pythonPackages.sphinx;
  coqPackages = pkgs.coqPackages_8_6;
in
pkgs.stdenv.mkDerivation {
  name = "haskell-env-1";
  buildInputs = [ghc
  emacsWithMyPackages
  sphinx
  llvm
#  polly
  git
  cabal2nix
  busybox
  pkgs.libressl
  coqPackages.ssreflect
  coqPackages.mathcomp
#  pkgs.coqPackages.contribs.all
#  coqPackages.domains
  coqPackages.QuickChick
  coqPackages.coq-ext-lib
#  coqPackages.unimath
  pkgs.compcert
  
  ];
  
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
