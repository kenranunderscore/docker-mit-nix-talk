{ pkgs ? import <nixpkgs> { } }:

let demo = import ../python_client { inherit pkgs; };
in pkgs.dockerTools.buildImage {
  name = "python-demo";
  tag = "latest";
  config = {
    # Start the python demo client as the CMD of the image
    Cmd = "${demo}/bin/demo";
  };
}
