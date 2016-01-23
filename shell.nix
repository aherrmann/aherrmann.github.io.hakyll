with import <nixpkgs> { };

let
  haskell = haskellPackages.ghcWithHoogle (pkgs: with pkgs; [
    cabal-install
    datetime
    ghc-mod
    hakyll
  ]);
in

runCommand "dummy" {
  buildInputs = [
    haskell
  ];
} ""
