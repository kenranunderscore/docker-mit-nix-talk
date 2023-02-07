{
  description = "An Active Group presentation";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
    revealjs = {
      url = "github:hakimel/reveal.js";
      flake = false;
    };
    mathjax = {
      url = "github:mathjax/mathjax";
      flake = false;
    };
    plantumlC4 = {
      url = "github:plantuml-stdlib/c4-plantuml";
      flake = false;
    };
    plantumlEIP = {
      url = "github:plantuml-stdlib/EIP-PlantUML";
      flake = false;
    };
    decktape = {
      url = "github:astefanutti/decktape";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    let
      supportedSystems = with flake-utils.lib.system; [
        x86_64-linux
        x86_64-darwin
        aarch64-darwin
      ];
    in flake-utils.lib.eachSystem supportedSystems (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        # A development shell for this talk/project.  Enter via
        # `nix develop`.
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            # Go
            go
            # Python
            python310
            poetry
            # Haskell
            ghc
            cabal-install
            # Libraries needed at runtime (not necessary when using
            # Nix to the fullest, i.e., using the Haskell packages
            # that are "nixified" and thus come with all their
            # transitive dependencies included)
            zlib
          ];
        };

        # The packages we can build; this includes the docker images.
        packages = {
          goBackend = import ./nix/go-docker-image.nix { inherit pkgs; };
          haskellBackend =
            import ./nix/haskell-docker-image.nix { inherit pkgs; };
          pythonClient =
            import ./nix/python-client-docker-image.nix { inherit pkgs; };
          allBackendsInOne =
            import ./nix/all-backends-in-one.nix { inherit pkgs; };
          release = import ./nix/docker-release.nix { inherit pkgs; };
        };

        # The stuff below is a template I've created to produce the
        # reveal.js slides from a .org file, as well as the PDF
        # conversion.  It has nothing to do with the actual demo
        # project.
        apps = {
          # The default target for `nix run`.  This builds the
          # reveal.js slides.
          default = let
            emacs = pkgs.emacs.pkgs.withPackages (p: [
              p.org-re-reveal
              p.gnuplot
              p.gnuplot-mode
              p.dockerfile-mode
              p.nix-mode
            ]);
            app = pkgs.writeShellScript "org-re-reveal" ''
              if [ ! -e plantuml/plugins ]; then
                mkdir -p plantuml/plugins
                ln -snf ${inputs.plantumlC4}/*.puml plantuml/plugins/.
                echo Symlinked PlantUML C4 to ./plantuml/plugins
                ln -snf ${inputs.plantumlEIP}/dist/*.puml plantuml/plugins/.
                echo Symlinked PlantUML EIP to ./plantuml/plugins
              fi

              export REVEAL_ROOT="${inputs.revealjs}"
              export REVEAL_MATHJAX_URL=
              export PATH=${pkgs.plantuml}/bin:${pkgs.gnuplot}/bin:$PATH
              echo $@
              ${emacs}/bin/emacs --batch -q -l export.el \
                  --eval="(org-re-reveal-export-file \"$@\" \"${inputs.revealjs}\" \"${inputs.mathjax}/es5/tex-chtml.js\")"
            '';
          in {
            type = "app";
            program = "${app}";
          };

          # May be used to create the PDF version of the talk.  See
          # the presentation's Makefile for an actual invocation.
          decktape = let
            decktapeWithDependencies = pkgs.stdenv.mkDerivation {
              name = "decktape-with-dependencies";
              src = inputs.decktape;
              buildInputs = [ pkgs.nodejs ];
              buildPhase = "HOME=$TMP npm install";
              installPhase = "cp -r . $out";
            };
            app = pkgs.writeShellScript "run-decktape"
              "${pkgs.nodejs}/bin/node ${decktapeWithDependencies}/decktape.js $@";
          in {
            type = "app";
            program = "${app}";
          };
          pdfunite = let poppler = pkgs.poppler_utils;
          in {
            type = "app";
            program = "${poppler}/bin/pdfunite";
          };
        };

      });
}
