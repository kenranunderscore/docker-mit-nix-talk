{ pkgs ? import <nixpkgs> { } }:

# Convert the .cabal file to a Nix expression that describes the
# project in a fully nix-compatible way.
pkgs.haskellPackages.callCabal2nix "haskell-backend" ./. { }
