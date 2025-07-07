{
  description = "dt-tools devel and build";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

  # shell.nix compatibility
  inputs.flake-compat.url = "https://flakehub.com/f/edolstra/flake-compat/1.tar.gz";

  inputs.self.submodules = true;

  outputs = { nixpkgs, ... }:
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
          default = pkgs.rustPlatform.buildRustPackage {
            pname = "dt-tools";
            version = (builtins.fromTOML (builtins.readFile ./Cargo.toml)).workspace.package.version;

            src = ./.;

            cargoLock.lockFile = ./Cargo.lock;

            meta = {
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
              cargo-nextest

              # It needs llvm-profdata and llvm-cov from bintools, but doesn't currently pick them up from PATH:
              # https://github.com/mozilla/grcov/issues/848
              # Normally they get added to the shell's PATH by llvmPackages.bintools's setup hook
              (writeShellScriptBin "grcov" ''
                exec ${grcov}/bin/grcov --llvm-path ${llvmPackages.llvm}/bin "$@"
              '')
              fd
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
