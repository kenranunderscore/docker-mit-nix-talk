{ pkgs ? import <nixpkgs> { } }:

let
  haskellBackend = import ../haskell_backend { inherit pkgs; };
  goBackend = import ../go_backend { inherit pkgs; };
  runScript = pkgs.writeShellScript "run" ''
    ${haskellBackend}/bin/haskell-backend &
    ${goBackend}/bin/server
  '';
in pkgs.dockerTools.buildImage {
  name = "both-backends";
  tag = "latest";
  config.Cmd = runScript;
}
