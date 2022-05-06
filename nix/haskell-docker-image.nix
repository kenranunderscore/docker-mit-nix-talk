{ pkgs ? import <nixpkgs> { } }:

let haskellBackend = import ../haskell_backend { inherit pkgs; };
in pkgs.dockerTools.buildImage {
  name = "haskell-backend";
  tag = "latest";
  config = {
    # Start the Haskell service as the CMD of the image
    Cmd = "${haskellBackend}/bin/haskell-backend";
  };
}
