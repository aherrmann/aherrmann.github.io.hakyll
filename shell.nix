{ system ? builtins.currentSystem
, nixpkgs ? <nixpkgs>
, pkgs ? (import nixpkgs { inherit system; })
, compiler ? "default"
, withProfiling ? false
, withHoogle ? false
}:

with pkgs;

let
  haskellPackages =
    let
      hp =
        if compiler == "default"
          then pkgs.haskellPackages
          else pkgs.haskell.packages.${compiler};
      addProfiling = hp:
        hp.override {
          overrides = self: super: {
            mkDerivation = args: super.mkDerivation (args // {
              enableLibraryProfiling = true;
            });
          };
        };
      addHoogle = hp:
        hp.override {
          overrides = self: super: {
            ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
            ghcWithPackages = self.ghc.withPackages;
          };
        };
      applyIf = cond: f: x: if cond then f x else x;
    in
      applyIf withProfiling addProfiling (
        applyIf withHoogle addHoogle (
          hp
        )
      );

  ghc =
    haskellPackages.ghcWithPackages (pkgs: with pkgs; [
      clay
      datetime
      hakyll
      hpygments
      hyphenation
    ]);
in
  pkgs.stdenv.mkDerivation {
    name = "blog-env";
    buildInputs = with pythonPackages; [
      ghc pygments pygments-markdown-lexer
    ];
    shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
  }
