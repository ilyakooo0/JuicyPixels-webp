{
  config,
  pkgs,
  lib,
  ...
}:

let
  LIMA_CIDATA_MNT = "/mnt/lima-cidata";
  LIMA_CIDATA_DEV = "/dev/disk/by-label/cidata";

  cfg = config.services.lima;

  lima-init-script = pkgs.runCommand "lima-init" { } ''
    cp ${
      pkgs.replaceVars ./lima-init.sh {
        LIMA_CIDATA_MNT = LIMA_CIDATA_MNT;
        PATH_DEPS = lib.makeBinPath [
          pkgs.shadow
          pkgs.gawk
          pkgs.mount
        ];
      }
    } $out
    chmod +x $out
  '';
in
{
  options.services.lima = {
    enable = lib.mkEnableOption "Lima guest agent and init support";
  };

  config = lib.mkIf cfg.enable {
    systemd.services.lima-init = {
      script = "${lima-init-script}";
      description = "Reconfigure the system from Lima userdata on startup";
      after = [ "network-pre.target" ];
      path = with pkgs; [
        bash
        coreutils
        gnused
        gnugrep
      ];
      restartIfChanged = true;
      unitConfig = {
        X-StopOnRemoval = false;
        RequiresMountsFor = LIMA_CIDATA_MNT;
      };
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
      };
    };

    systemd.services.lima-guestagent = {
      description = "Forward ports to the Lima host agent";
      wantedBy = [ "multi-user.target" ];
      after = [
        "network.target"
        "lima-init.service"
      ];
      requires = [ "lima-init.service" ];
      script = ''
        while read -r line; do export "$line"; done <"${LIMA_CIDATA_MNT}"/lima.env
        ${LIMA_CIDATA_MNT}/lima-guestagent daemon --vsock-port "$LIMA_CIDATA_VSOCK_PORT"
      '';
      serviceConfig = {
        Type = "simple";
        Restart = "on-failure";
      };
    };

    fileSystems."${LIMA_CIDATA_MNT}" = {
      device = LIMA_CIDATA_DEV;
      fsType = "auto";
      options = [
        "ro"
        "mode=0700"
        "dmode=0700"
        "overriderockperm"
        "exec"
        "uid=0"
      ];
    };

    environment.etc.environment.source = "${LIMA_CIDATA_MNT}/etc_environment";

    networking.nat.enable = true;

    environment.systemPackages = with pkgs; [
      bash
      sshfs
      fuse3
      git
    ];

    boot.kernel.sysctl = {
      "kernel.unprivileged_userns_clone" = 1;
      "net.ipv4.ping_group_range" = "0 2147483647";
      "net.ipv4.ip_unprivileged_port_start" = 0;
    };
  };
}
