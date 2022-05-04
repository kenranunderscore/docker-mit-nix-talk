{ pkgs ? import <nixpkgs> { } }:

# Similar to callCabal2nix or poetry2nix, this enables us to point to
# a Go project and build it with Nix.  Use 
pkgs.buildGoModule {
  src = pkgs.lib.cleanSource ./.;
  pname = "go_backend";
  version = "0.1.0";
  # You can use this fake SHA256 to get Nix to tell you the real one
  # on build:
  # vendorSha256 = pkgs.lib.fakeSha256;
  vendorSha256 = "sha256-pQpattmS9VmO3ZIQUFn66az8GSmB4IvYhTTCFn6SUmo=";
}
