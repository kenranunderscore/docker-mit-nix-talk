{ pkgs ? import <nixpkgs> { } }:

# Sadly this is broken in the current nixpkgs master branch just now:
# https://github.com/nix-community/poetry2nix/issues/606
# But fixed in poetry2nix's master branch.
# pkgs.poetry2nix.mkPoetryApplication {
#   projectDir = ./.;
#   python = pkgs.python310;
# }

# # As a workaround, we can get a bleeding-edge poetry2nix ourselves:
# let
#   poetry2nix = import (pkgs.fetchFromGitHub {
#     owner = "nix-community";
#     repo = "poetry2nix";
#     rev = "6ed6894f58177179f737a820f79e3e04fe0ec0d5";
#     sha256 = "sha256-iY7KGz7HbUUxvM/e0fL8p27eHWuI4wMm/S/iIdYxXrU=";
#   }) {
#     inherit pkgs;
#     inherit (pkgs) poetry;
#   };
# in poetry2nix.mkPoetryApplication {
#   projectDir = ./.;
#   python = pkgs.python310;
# }

# But this would be a way to get it working for simple/small scripts
# manually:
let
  # We can easily build something resembling a virtualenv by
  # requesting "a Python with a list of known packages".
  myPython = pkgs.python310.withPackages (p: [ p.loguru p.requests ]);
  # Then build a shell script that calls our main function with the
  # Python above that knows about the dependencies.  Of course it
  # would be nicer if they were inferred, see above.
in pkgs.writeShellScript "demo" ''
  ${myPython}/bin/python ${./python_client}/__init__.py
''
