{ pkgs ? import <nixpkgs> { } }:

pkgs.poetry2nix.mkPoetryApplication {
  projectDir = ./.;
  python = pkgs.python310;
}
