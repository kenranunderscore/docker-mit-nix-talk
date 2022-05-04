{ pkgs ? import <nixpkgs> { } }:

let
  bothBackends = import ./both-backends-one-image.nix { inherit pkgs; };
  pythonDemo = import ./python-client-docker-image.nix { inherit pkgs; };
in pkgs.dockerTools.mergeImages [ bothBackends pythonDemo ]
