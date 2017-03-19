let

  nixpkgs = (import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "ade58373509ec32b7ab72040986e93aa2d074c74";
    sha256 = "06d68l22qk1hrcx5mvw6i02j5ijgcv7yyjwz4kxa0x274is81mrh";
  };

  hsLib =
    let
      pkgs = import nixpkgs {};
      path = "${nixpkgs}/pkgs/development/haskell-modules/lib.nix";
    in
    import (builtins.toPath path) { inherit pkgs; }
  ;

  hsOverlay = self: super: {
    haskellPackages = super.haskellPackages.override {
      overrides = self': super': {
        datetime =
          let
            patch = self.writeText "cabal.patch" ''
              diff --git a/datetime.cabal b/datetime.cabal
              index 169f3b6..615ca50 100644
              --- a/datetime.cabal
              +++ b/datetime.cabal
              @@ -42,7 +42,7 @@ test-suite test
                     , old-time >= 1.0.0.1
                     , time >= 1.1.2.2

              -      , base >=4.2 && <4.9
              +      , base < 5
                     , test-framework
                     , HUnit
                     , QuickCheck
            '';
          in
          hsLib.appendPatch super'.datetime patch
        ;
        hyphenation =
          let
            patch = self.writeText "cabal.patch" ''
              diff --git a/hyphenation.cabal b/hyphenation.cabal
              index c3239ba..b225833 100644
              --- a/hyphenation.cabal
              +++ b/hyphenation.cabal
              @@ -78,8 +78,8 @@ test-suite doctests
                 build-depends:
                   base,
                   containers,
              -    directory >= 1.0 && < 1.3,
              -    doctest   >= 0.8 && < 0.10,
              +    directory >= 1.0 && < 1.4,
              +    doctest   >= 0.8 && < 0.12,
                   filepath  >= 1.3 && < 1.5,
                   hyphenation,
                   unordered-containers
            '';
          in
          hsLib.appendPatch super'.hyphenation patch
        ;
        clay =
          let
            patch = self.writeText "cabal.patch" ''
              diff --git a/clay.cabal b/clay.cabal
              index 29c9e78..6569835 100644
              --- a/clay.cabal
              +++ b/clay.cabal
              @@ -83,5 +83,5 @@ Test-Suite Test-Clay
                   mtl                  >= 1     && < 2.3,
                   text                 >= 0.11  && < 1.3,
                   hspec-expectations   >= 0.7.2 && < 0.9,
              -    hspec                >= 2.2.0 && < 2.3
              +    hspec                >= 2.2.0 && < 2.5
                 Ghc-Options: -Wall
            '';
          in
          hsLib.appendPatch super'.clay patch
        ;
        cabal-bounds =
          let
            patch = self.writeText "cabal.patch" ''
              diff --git a/cabal-bounds.cabal b/cabal-bounds.cabal
              index 81fbac1..91548f8 100644
              --- a/cabal-bounds.cabal
              +++ b/cabal-bounds.cabal
              @@ -81,7 +81,7 @@ library
                       cabal-lenses >=0.4.6 && <0.5,
                       Cabal >=1.18.0 && <1.25,
                       filepath >=1.3 && <1.5,
              -        directory ==1.2.*
              +        directory >=1.2 && <1.4
                   cpp-options: -DCABAL
                   hs-source-dirs: lib
                   other-modules:
            '';
          in
          hsLib.appendPatch super'.cabal-bounds patch
        ;
      };
    };
  };

in

with (import nixpkgs { overlays = [ hsOverlay ]; });

let

  ghc =
    haskellPackages.ghcWithPackages (pkgs: with pkgs; [
      clay
      datetime
      hakyll
      hyphenation
    ]);

in

pkgs.stdenv.mkDerivation {
  name = "blog-env";
  buildInputs = with pythonPackages; [
    haskellPackages.cabal-bounds cabal-install ghc
    pygments pygments-markdown-lexer
  ];
  shellHook = ''
    eval $(egrep ^export ${ghc}/bin/ghc)
    PS1='nix:\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]$(__git_ps1 "(%s)")\$ '
  '';
}
