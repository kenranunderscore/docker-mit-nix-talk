let
  pkgs = import (builtins.fetchTarball {
    name = "my-nixpkgs-pin";
    url =
      "https://github.com/nixos/nixpkgs/archive/87d34a6b8982e901b8e50096b8e79ebc0e66cda0.tar.gz";
    sha256 = "sha256:0dqjw05vbdf6ahy71zag8gsbfcgrf7fxz3xkwqqwapl0qk9xk47a";
  }) { };
  bothBackends = import ./all-backends-in-one.nix { inherit pkgs; };
  pythonDemo = import ./python-client-docker-image.nix { inherit pkgs; };
in pkgs.dockerTools.mergeImages [ bothBackends pythonDemo ]
