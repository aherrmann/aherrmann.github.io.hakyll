with import <nixpkgs> { };

let
  haskell = haskellPackages.ghcWithHoogle (pkgs: with pkgs; [
    cabal-install
    hakyll
  ]);
in

runCommand "dummy" {
  buildInputs = [
    haskell
  ];
} ""
