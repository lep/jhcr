{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default";
    common-j.url = "github:lep/common-j";
    common-j.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, systems, common-j }:
    let
      eachSystem = nixpkgs.lib.genAttrs (import systems);
      jhcr = pkgs:
        let
          cabal-stuff = pkgs.haskellPackages.callPackage ./jhcr.nix { };
          convert = pkgs.haskell.lib.setBuildTarget cabal-stuff "convert";
          jhcr' = pkgs.haskell.lib.setBuildTarget cabal-stuff "jhcr";
          jhcr'' = jhcr'.overrideAttrs (final: prev: {
            env = (prev.env or { }) // { PATCH_LVL = 133; };
            preBuild = ''
              ${convert}/bin/convert ${common-j}/common.j
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
        in { default = jhcr pkgs; });

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

