{
  description = "dt-tools devel and build";

  # Unstable required until Rust 1.85 (2024 edition) is on stable
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  # shell.nix compatibility
  inputs.flake-compat.url = "https://flakehub.com/f/edolstra/flake-compat/1.tar.gz";

  outputs = { self, nixpkgs, ... }:
    let
      # System types to support.
      targetSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

      # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
      forAllSystems = nixpkgs.lib.genAttrs targetSystems;
    in {
      packages = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = pkgs.rustPlatform.buildRustPackage rec {
            pname = "dt-tools";
            version = (builtins.fromTOML (builtins.readFile ./Cargo.toml)).workspace.package.version;

            src = assert nixpkgs.lib.assertMsg (self.submodules == true)
              "Unable to build without submodules. Append '?submodules=1#' to the URL.";
              ./.;

            cargoLock.lockFile = ./Cargo.lock;

            meta = with nixpkgs.lib; {
              description = "Modern devicetree tools in Rust";
              homepage = "https://github.com/axelkar/dt-tools";
            };
          };
        }
      );
      devShells = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = pkgs.mkShell {
            strictDeps = true;
            RUST_SRC_PATH = "${pkgs.rustPlatform.rustLibSrc}";
            RUSTFLAGS = "-C link-arg=-fuse-ld=lld";
            nativeBuildInputs = with pkgs; [
              cargo
              rustc
              llvmPackages.bintools # LLD

              rustfmt
              clippy
              rust-analyzer

              cargo-release
            ];
          };
          vscode = pkgs.mkShellNoCC {
            strictDeps = true;
            nativeBuildInputs = with pkgs; [
              nodejs_23
              vscodium
              vsce
            ];
          };
        }
      );
    };
}
