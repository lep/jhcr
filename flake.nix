{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    jassdoc = {
      url = "github:lep/jassdoc";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.systems.follows = "systems";
    };

    systems.url = "github:nix-systems/default";
  };

  outputs = { self, nixpkgs, systems, jassdoc }:
    let
      eachSystem = nixpkgs.lib.genAttrs (import systems);
      jhcr = { pkgs, system }:
        let
          cabal-stuff = pkgs.haskellPackages.callPackage ./jhcr.nix { };
          convert = pkgs.haskell.lib.setBuildTarget cabal-stuff "convert";
          jhcr' = pkgs.haskell.lib.setBuildTarget cabal-stuff "jhcr";
          jhcr'' = jhcr'.overrideAttrs (final: prev: {
            meta.mainProgram = "jhcr";
            env = (prev.env or { }) // { PATCH_LVL = 133; };
            preBuild = ''
              ${pkgs.lib.getExe' convert "convert"} ${
                jassdoc.packages.${system}.jass-files
              }/common.j
              mkdir out
              for j in src/runtime/*.j; do
                bash src/process.sh "$j" "''${j/src\/runtime/out}" JHCR_
              done
            '';
          });
        in jhcr'';
    in {
      packages = eachSystem (system:
        let pkgs = import nixpkgs { inherit system; };
        in { default = jhcr { inherit system pkgs; }; });

      devShells = eachSystem (system:
        let pkgs = import nixpkgs { inherit system; };
        in {
          default = pkgs.mkShell {
            env.PATCH_LVL = 133;
            # packages = [ ];
            nativeBuildInputs = [ pkgs.cabal-install ];
          };
        });
    };
}

