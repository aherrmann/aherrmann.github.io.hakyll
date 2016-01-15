with import <nixpkgs> { };

let
  haskell = haskellPackages.ghcWithHoogle (pkgs: with pkgs; [
    hakyll
  ]);
in

runCommand "dummy" {
  buildInputs = [
    haskell
  ];
} ""
