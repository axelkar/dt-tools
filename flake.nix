{
  description = "dt-tools devel";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShellNoCC {
          env.RUSTFLAGS = "-C link-arg=-fuse-ld=lld";
          nativeBuildInputs = [ pkgs.llvmPackages.bintools ];
        };
      });
}
