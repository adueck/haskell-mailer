{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/a58390ab6f1a.tar.gz") {}
}:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskell.compiler.ghc96 
    pkgs.haskellPackages.cabal-install
    pkgs.zlib
    pkgs.postgresql # not sure if I need this too or just libpq
    pkgs.libpq
  ];
}
