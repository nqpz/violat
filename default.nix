with (import <nixpkgs> { });
let
  # This is the last commit before ghc865 was removed.
  pkgs_ghc865 = import (builtins.fetchTarball {
    name = "nixpkgs-unstable-ghc844";
    url = "https://github.com/nixos/nixpkgs/archive/51cfea6e6c4ee557c2398a6815704eb8f39caf46.tar.gz";
    sha256 = "0ivxs6awrvgz2gn5jjhrvyd3lwx74xdjasj6a10zv7hh2rsnp509";
  }) {};
in
haskell.lib.buildStackProject {
  name = "violat";
  buildInputs = with pkgs_ghc865; [ pkgconfig SDL SDL_gfx SDL_ttf SDL_mixer SDL_image cairo zlib ];
  ghc = pkgs_ghc865.haskell.compiler.ghc865;
  src = ./.;
}
