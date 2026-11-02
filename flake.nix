{
  description = "dt-tools devel and build";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

  # shell.nix compatibility
  inputs.flake-compat.url = "https://flakehub.com/f/edolstra/flake-compat/1.tar.gz";

  inputs.dt-schema = {
    url = "github:devicetree-org/dt-schema";
    flake = false;
  };

  inputs.crane.url = "github:ipetkov/crane";

  inputs.advisory-db = {
    url = "github:rustsec/advisory-db";
    flake = false;
  };

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs =
    {
      self,
      nixpkgs,
      dt-schema,
      crane,
      advisory-db,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        linuxSrc =
          let
            linuxVersion = "7.3";
            linuxSrcRaw = pkgs.fetchurl {
              url = "mirror://kernel/linux/kernel/v${nixpkgs.lib.versions.major linuxVersion}.x/linux-${linuxVersion}.tar.xz";
              hash = "sha256-hzY8JPEOBjIes2J2VSEJTpRHH2v/i84taz5A858ASVM=";
            };
          in
          pkgs.runCommand "linux-${linuxVersion}-src" { } ''
            mkdir "$out"
            tar -xf "${linuxSrcRaw}" --strip-components=1 -C "$out"
          '';

        linuxBindings = "${linuxSrc}/Documentation/devicetree/bindings";

        craneLib = crane.mkLib pkgs;

        commonArgs = {
          pname = "dt-tools";
          src = craneLib.cleanCargoSource ./.;
          strictDeps = true;
          env = {
            DT_TOOLS_LINUX_BINDINGS = linuxBindings;
            DT_TOOLS_DT_SCHEMA_REPO = dt-schema.outPath;
          };
        };

        packageArgs = commonArgs // {
          # don't build lsp-auto-runner development tool
          cargoExtraArgs = "--package dt-tools";
        };

        cargoArtifacts = craneLib.buildDepsOnly commonArgs;

        dt-tools = craneLib.buildPackage (
          packageArgs
          // {
            inherit cargoArtifacts;

            doCheck = false;
            meta = {
              description = "Modern devicetree tools in Rust";
              homepage = "https://github.com/axelkar/dt-tools";
            };
          }
        );
      in
      {
        packages = {
          default = dt-tools;

          # TODO: VS Code extension
          # TODO: static musl executable for releases
        };

        checks = {
          inherit dt-tools;

          # Run tests
          tests = craneLib.cargoTest (
            commonArgs
            // {
              # `cleanCargoSource` would drop test data such as crates/parser/test_data
              src = ./.;

              cargoArtifacts = cargoArtifacts;
              nativeBuildInputs = [ pkgs.dtc ];
            }
          );

          # Check code using Clippy
          clippy = craneLib.cargoClippy (
            commonArgs
            // {
              # `--all-targets` compiles the tests, so include their test data
              src = ./.;

              inherit cargoArtifacts;
              cargoClippyExtraArgs = "--all-targets -- --deny warnings";
            }
          );

          # Check and build documentation
          doc = craneLib.cargoDoc (
            commonArgs
            // {
              inherit cargoArtifacts;
              # This can be commented out or tweaked as necessary, e.g. set to
              # `--deny rustdoc::broken-intra-doc-links` to only enforce that lint
              env.RUSTDOCFLAGS = "--deny warnings";
            }
          );

          # Check formatting
          fmt = craneLib.cargoFmt {
            inherit (commonArgs) pname src;
          };

          # Check TOML formatting
          toml-fmt = craneLib.taploFmt {
            inherit (commonArgs) pname;
            src = pkgs.lib.sources.sourceFilesBySuffices commonArgs.src [ ".toml" ];
          };

          # Audit dependencies
          audit = craneLib.cargoAudit {
            inherit (commonArgs) pname src;
            inherit advisory-db;
          };

          # Audit licenses
          deny = craneLib.cargoDeny {
            inherit (commonArgs) pname src;
          };
        };
        devShells = {
          default = craneLib.devShell {
            strictDeps = true;

            # Inherit inputs from checks.
            checks = self.checks.${system};
            packages = with pkgs; [
              llvmPackages.bintools # LLD

              rust-analyzer

              cargo-release
              cargo-nextest

              # It needs llvm-profdata and llvm-cov from bintools, but doesn't currently pick them up from PATH:
              # https://github.com/mozilla/grcov/issues/848
              # Normally they get added to the shell's PATH by llvmPackages.bintools's setup hook
              (writeShellScriptBin "grcov" ''
                exec ${grcov}/bin/grcov --llvm-path ${llvmPackages.llvm}/bin "$@"
              '')

              # For tests and scripts
              dtc
              gcc
              clang
              hyperfine
              fd
            ];

            env.RUST_SRC_PATH = "${pkgs.rustPlatform.rustLibSrc}"; # For rust-analyzer
            env.RUSTFLAGS = "-C link-arg=-fuse-ld=lld"; # Faster linker
            env.LINUX = linuxSrc;
            env.DT_TOOLS_LINUX_BINDINGS = linuxBindings;
            env.DT_TOOLS_DT_SCHEMA_REPO = dt-schema.outPath;
          };
          vscode = pkgs.mkShellNoCC {
            strictDeps = true;
            nativeBuildInputs = with pkgs; [
              nodejs_latest
              vscodium
              vsce
            ];
          };
        };
      }
    );
}
