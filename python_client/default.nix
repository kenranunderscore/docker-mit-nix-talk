{ pkgs ? import <nixpkgs> { } }:

# Sadly this is broken just now:
# https://github.com/nix-community/poetry2nix/issues/606
# pkgs.poetry2nix.mkPoetryApplication {
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
