{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachSystem
      [
        flake-utils.lib.system.x86_64-linux
        flake-utils.lib.system.aarch64-linux
      ]
      (
        system:
        let
          nixos = nixpkgs.lib.nixosSystem {
            inherit system;
            modules = [
              ./nix/lima-init.nix
              (
                {
                  config,
                  lib,
                  pkgs,
                  modulesPath,
                  ...
                }:
                {
                  imports = [
                    "${modulesPath}/profiles/qemu-guest.nix"
                  ];

                  # Boot
                  boot.loader.grub = {
                    enable = true;
                    efiSupport = true;
                    efiInstallAsRemovable = true;
                    device = "nodev";
                  };
                  boot.growPartition = true;
                  boot.kernelParams = [ "console=tty0" ];
                  boot.kernelPackages = pkgs.linuxPackages_latest;

                  # Filesystems
                  fileSystems."/" = {
                    device = "/dev/disk/by-label/nixos";
                    fsType = "ext4";
                    autoResize = true;
                    options = [
                      "noatime"
                      "nodiratime"
                      "discard"
                    ];
                  };
                  fileSystems."/boot" = {
                    device = lib.mkForce "/dev/vda1";
                    fsType = "vfat";
                  };

                  # Nix
                  nixpkgs.config.allowUnfree = true;
                  nix.settings = {
                    experimental-features = [
                      "nix-command"
                      "flakes"
                    ];
                    trusted-users = [ "@wheel" ];
                    always-allow-substitutes = true;
                    extra-substituters = [ "https://cache.garnix.io" ];
                    extra-trusted-public-keys = [ "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g=" ];
                    netrc-file = "/lima/etc/nix/netrc";
                  };

                  # Networking
                  networking.hostName = "agent-nixos";

                  # Services
                  services.lima.enable = true;
                  services.openssh.enable = true;

                  # Security
                  security.sudo.wheelNeedsPassword = false;

                  # Shell
                  programs.fish = {
                    enable = true;
                    shellAliases.dev = "nix develop --command fish";
                    promptInit = ''
                      function fish_prompt
                        set -l cwd (prompt_pwd)
                        set -l user (whoami)
                        set -l nix_indicator ""
                        if set -q IN_NIX_SHELL; or test -n "$DEVSHELL_DIR"
                          set nix_indicator "❄️ "
                        end
                        echo -n (set_color brmagenta)'[agent-nixos]' (set_color brgreen)"$user" (set_color brblue)"$cwd" (set_color normal)"$nix_indicator"'> '
                      end
                    '';
                  };

                  # Users
                  users.defaultUserShell = pkgs.fish;
                  users.users.root.initialPassword = "nixos";

                  # Packages
                  environment.systemPackages = with pkgs; [
                    # Editors
                    helix
                    vim

                    # Development
                    cabal-install
                    claude-code
                    git
                    jujutsu
                    stack
                    (python3.withPackages (ps: with ps; [
                      httpx
                      beautifulsoup4
                      fastmcp
                    ]))

                    # CLI tools
                    bash
                    btop
                    bat
                    curlFull
                    delta
                    fd
                    fzf
                    jq
                    ripgrep
                    tree
                    wget

                    # Terminal
                    kitty.terminfo
                  ];

                  system.stateVersion = "25.11";

                  # Image build
                  system.build.qcow2 = import "${modulesPath}/../lib/make-disk-image.nix" {
                    inherit config lib pkgs;
                    name = "agent-nixos-${system}";
                    baseName = "agent-nixos-${system}";
                    diskSize = "auto";
                    format = "qcow2";
                    partitionTableType = "efi";
                    installBootLoader = true;
                    memSize = 2048;
                  };
                }
              )
            ];
          };
        in
        {
          packages.default = nixos.config.system.build.qcow2;
        }
      );
}
