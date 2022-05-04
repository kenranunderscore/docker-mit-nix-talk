{ pkgs ? import <nixpkgs> { } }:

let
  goBackend = import ../go_backend { inherit pkgs; };
  # This is the shorter form of:
  # goBackend = import ../go_backend/default.nix { inherit pkgs; };
in pkgs.dockerTools.buildImage {
  name = "go-backend";
  tag = "latest";
  # Impure: it changes the timestamp every time a rebuild is needed.
  # created = "now";
  # For debugging this is sometimes nice/necessary:
  contents = [ pkgs.bash pkgs.coreutils ];
  config = {
    # Start the Go service as the CMD of the image
    Cmd = "${goBackend}/bin/server";
  };
}
