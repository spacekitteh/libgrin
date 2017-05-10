{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithHoogle (ps: with ps; 
[ ghc-mod
 hspec
 hspec-megaparsec
 hspec-laws
 hspec-expectations-lens
 hspec-checkers
 hspec-smallcheck
 freer-effects
 lens
 megaparsec
 hoopl
 constraints
 QuickCheck
 ansi-wl-pprint 
 criterion
 haskintex
 freer
 text
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
 llvm-hs
# llvm-hs-typed
# llvm-hs-pretty

 megaparsec
 idris
 structured-haskell-mode
]);
   
 emacsWithMyPackages = let customEmacsPackages = pkgs.emacsPackagesNg.overrideScope (super: self: {
      emacs = pkgs.emacs.override { withGTK2 = true; withGTK3 = false; }; }); in
      customEmacsPackages.emacsWithPackages 
        (with pkgs.emacsPackagesNg;
          [ ghc-mod
            haskell-mode
            structured-haskell-mode
            rainbow-delimiters
            company-ghc
            flycheck-haskell
            org-plus-contrib
            idris-mode
            org-trello            
            auctex
            magit ]);

  llvm = pkgs.llvm_39;
  git = pkgs.git;
  cabal2nix = pkgs.cabal2nix;
  busybox = pkgs.busybox;
  sphinx = pkgs.pythonPackages.sphinx;
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
  ];
  
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
