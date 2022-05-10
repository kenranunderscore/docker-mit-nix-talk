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
        # A development shell in which it's possible to build/develop
        # the presentation.
        devShells.default = pkgs.mkShell {
          # Environment variables pointing to reveal.js and MathJax
          # inside the Nix store.
          REVEAL_ROOT = "${inputs.revealjs}";
          REVEAL_MATHJAX_URL = "${inputs.mathjax}/es5/tex-chtml.js";

          # We need Emacs and PlantUML to "build" presentations.
          buildInputs = let
            emacs =
              pkgs.emacsWithPackages (p: [ p.org-re-reveal p.dockerfile-mode ]);
          in [ emacs pkgs.plantuml ];

          # Symlink the PlantUML C4 libraries to a local directory.
          # TODO: regenerate the symlink if the lock (and thus the
          # store path) changes.
          shellHook = ''
            if [ ! -e plantuml/plugins ]; then
              ln -snf ${inputs.plantumlC4} plantuml/plugins
              echo Symlinked PlantUML C4 to ./plantuml/plugins
            fi
          '';
        };
      });
}
