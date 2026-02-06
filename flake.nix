{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    stacklock2nix.url = "github:cdepillabout/stacklock2nix";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      stacklock2nix,
      treefmt-nix,
      ...
    }:
    let
      supportedSystems = flake-utils.lib.defaultSystems;
    in
    flake-utils.lib.eachSystem supportedSystems (
      system:
      let
        overlays = [
          stacklock2nix.overlay
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          config.allowUnfree = true;
        };

        # ============================================================
        # Haskell Build Infrastructure
        # ============================================================
        compilerVersion = "9103";
        all-cabal-nixes =
          let
            tarball = pkgs.fetchurl {
              url = "https://github.com/all-cabal-nixes/all-cabal-nixes/archive/df3654befebccb47544e1827a533c8fd5bb78db5.tar.gz";
              sha256 = "sha256-WsNmTswAKFbAruk9yoKRHI6sJ/e5jtMPme9d3B9UWnM=";
            };
          in
          pkgs.runCommand "all-cabal-hashes"
            {
              preferLocalBuild = true;
              allowSubstitutes = false;
            }
            ''
              mkdir -p $out
              tar -xzvf ${tarball} --strip-components=1 --exclude="*/Cassava/*" --exclude="*/Safe/*" -C $out
            '';

        haskellPkgSet =
          (pkgs.stacklock2nix {
            inherit all-cabal-nixes;
            stackYaml = ./stack.yaml;
            baseHaskellPkgSet = pkgs.haskell.packages."ghc${compilerVersion}";

            additionalHaskellPkgSetOverrides = pkgs.lib.composeManyExtensions [
              (hfinal: hprev: {
                JuicyPixels-webp = pkgs.lib.pipe hprev.JuicyPixels-webp (
                  with pkgs.haskell.lib;
                  [
                    dontCheck
                    dontHaddock
                    doStrip
                    disableLibraryProfiling
                    disableExecutableProfiling
                  ]
                );
              })
            ];
          }).pkgSet;

        # ============================================================
        # Code Formatting
        # ============================================================
        treefmtEval = treefmt-nix.lib.evalModule pkgs {
          projectRootFile = "flake.nix";

          programs.ormolu.enable = true;
          programs.nixfmt.enable = true;

          settings.formatter.ormolu.includes = [ "src/**/*.hs" ];
          settings.formatter.nixfmt.includes = [
            "*.nix"
            "nix/**/*.nix"
          ];
        };
      in
      rec {
        # ============================================================
        # Packages
        # ============================================================
        packages = {
          default = haskellPkgSet.JuicyPixels-webp;
        };

        # ============================================================
        # Development Shell
        # ============================================================
        devShells.default = pkgs.mkShell {
          buildInputs =
            with pkgs;
            [
              haskellPkgSet.ghc
              haskellPkgSet.haskell-language-server
              stack
              cabal-install
              zlib
            ]
            ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
              pkgs.apple-sdk
            ];
        };

        # ============================================================
        # Apps
        # ============================================================
        apps = {
          lima = {
            type = "app";
            meta.description = "Start or attach to the Lima coding agent VM";
            program =
              (pkgs.writeScript "lima" ''
                #!/bin/sh
                set -e

                name="''${1:-agent-nixos}"
                limactl="${pkgs.lima}/bin/limactl"
                flake_dir="$(${pkgs.coreutils}/bin/dirname "$(${pkgs.coreutils}/bin/readlink -f "$0")")/../.."

                status=$($limactl list --format='{{.Name}}:{{.Status}}' | ${pkgs.gnugrep}/bin/grep "^''${name}:" | ${pkgs.coreutils}/bin/cut -d: -f2 || true)

                case "$status" in
                  "")
                    $limactl start --yes --name="$name" \
                      --set=".param.project = \"$(pwd)\"" \
                      --set=".images[0].location = \"$(pwd)/lima/agent-nixos-aarch64-linux.qcow2\"" \
                      --set=".images[1].location = \"$(pwd)/lima/agent-nixos-x86_64-linux.qcow2\"" \
                      lima/agent-nixos.yaml
                    ;;
                  Running)
                    ;;
                  *)
                    $limactl start "$name"
                    ;;
                esac

                $limactl shell --workdir /project "$name"
              '').outPath;
          };
          limactl =
            flake-utils.lib.mkApp {
              drv = pkgs.lima;
              exePath = "/bin/limactl";
            }
            // {
              meta.description = "Lima VM management";
            };
        };

        # ============================================================
        # Formatting and Checks
        # ============================================================
        formatter = treefmtEval.config.build.wrapper;
        checks = {
          formatting = treefmtEval.config.build.check self;
        };
      }
    );

  nixConfig = {
    extra-substituters = [ "https://cache.garnix.io" ];
    extra-trusted-public-keys = [
      "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
    ];
    allow-import-from-derivation = true;
  };
}
