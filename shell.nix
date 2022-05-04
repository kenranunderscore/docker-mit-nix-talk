let pkgs = import <nixpkgs> { };

in pkgs.mkShell {
  nativeBuildInputs = [
    # Go
    pkgs.go
    # Python
    pkgs.python310
    pkgs.poetry
    # Haskell
    pkgs.ghc
    pkgs.cabal-install
    # Libraries needed at runtime (not necessary when using Nix to the
    # fullest)
    pkgs.zlib
  ];
}
