with import <nixpkgs> { };

let
  haskell = haskellPackages.ghcWithHoogle (pkgs: with pkgs; [
    cabal-install
    ghc-mod
    hakyll
  ]);
in

runCommand "dummy" {
  buildInputs = [
    haskell
  ];
} ""
