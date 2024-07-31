{
  description = "Your flake name";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system}; in
      {
        devShells.default =
          pkgs.mkShell {
            buildInputs = [
              pkgs.zig_0_12
              pkgs.zls
            ];
          };
      });
}
