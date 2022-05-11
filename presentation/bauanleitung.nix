{ pkgs }:

pkgs.stdenv.mkDerivation {
  pname = "my-example";
  version = "0.1.0";
  src = ./.;
  buildInputs = [ pkgs.gnumake pkgs.gcc ];
  buildPhase = "make";
  installPhase = ''
    make install
    cp -r myResult $out
  '';
}
