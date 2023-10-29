{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    common-j.url = "github:lep/common-j";
    common-j.inputs.nixpkgs.follows = "nixpkgs";
    common-j.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, common-j }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        packageName = "jhcr";

        ghcPackages = pkgs.haskellPackages.ghcWithPackages (ps: [
          ps.file-embed
          ps.filepath
          ps.directory
          ps.bytestring
          ps.binary
          ps.containers
          ps.hashable
          ps.megaparsec
          ps.optparse-applicative
          ps.gitrev
          ps.parser-combinators
          ps.lca
          ps.mtl
          ps.lens
          ps.dlist
          ps.unordered-containers
          ps.vector
          ps.utf8-string
        ]);

        jhcr = pkgs.stdenv.mkDerivation {
          name = "jhcr";
          src = self;
          buildPhase = ''
            export PATCH_LVL=133
		    
            ${ghcPackages}/bin/runhaskell -isrc src/convert.hs ${common-j}/common.j

            mkdir out
            for j in src/runtime/*.j; do
              bash src/process.sh "$j" "''${j/src\/runtime/out}" JHCR_
            done

            ${ghcPackages}/bin/ghc -O -isrc src/Main.hs -o jhcr
          '';


          installPhase = ''
            install -Dt $out/bin jhcr
          '';
        };

        buildInputs = [ pkgs.gnumake pkgs.cabal-install ghcPackages ];
      in rec {
        packages = { ${packageName} = jhcr; };

        defaultPackage = packages.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = buildInputs ++ [ pkgs.cabal-install ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}

