with import <nixpkgs> { };

let
  haskell = haskellPackages.ghcWithHoogle (pkgs: with pkgs; [
    cabal-install
    clay
    datetime
    ghc-mod
    hakyll
    hyphenation
  ]);
in

runCommand "dummy" {
  buildInputs = [
    haskell
  ];
} ""
