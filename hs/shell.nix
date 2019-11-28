{ ghc }:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "scripts-nix-shell";
  buildInputs = [ glpk pcre zlib.dev ];
}
