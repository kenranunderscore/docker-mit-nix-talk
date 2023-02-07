let
  pkgs = import <nixpkgs> { };
  bothBackends = import ./all-backends-in-one.nix { inherit pkgs; };
  pythonDemo = import ./python-client-docker-image.nix { inherit pkgs; };
in pkgs.dockerTools.mergeImages [ bothBackends pythonDemo ]
