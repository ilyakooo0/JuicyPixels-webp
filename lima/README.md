# Lima VM for Claude Code

NixOS virtual machine for running `claude --dangerously-skip-permissions` in an isolated Linux environment.

## Overview

This sets up a Lima VM running NixOS with Claude Code and all development tools pre-installed. The project directory is mounted read-write at `/project`, and host dotfiles (jj, helix, git, gnupg, `~/.claude`) are mounted into the guest.

## Usage

```bash
./dev/lima.sh
```

This will:
1. Build and start the VM on first run (or resume if stopped)
2. Drop you into an interactive shell at `/project`

Inside the VM:
```bash
claude --dangerously-skip-permissions
```

### Custom VM Name

```bash
./dev/lima.sh my-vm-name    # defaults to agent-nixos
```

## Building the VM Image

The VM image must be built before first use:

```bash
nix build ./lima#              # builds for current architecture
```

This produces a qcow2 disk image. The `dev/lima.sh` script expects the image at `lima/agent-nixos-<arch>.qcow2`.

## VM Configuration

| Setting | Value |
|---------|-------|
| CPUs | 8 |
| Memory | 16 GiB |
| Hostname | `agent-nixos` |
| Shell | fish |
| SSH agent | Forwarded |

### Mounts

| Host | Guest | Writable |
|------|-------|----------|
| `~/.config/jj` | `~/.config/jj` | No |
| `~/.config/helix` | `~/.config/helix` | No |
| `~/.config/git` | `~/.config/git` | No |
| `~/.gnupg` | `~/.gnupg` | No |
| `~/.claude` | `~/.claude` | Yes |
| `/etc/nix` | `/lima/etc/nix` | No |
| Project root | `/project` | Yes |

### Pre-installed Packages

Editors: helix, vim. Development: cabal-install, claude-code, git, jujutsu, stack. CLI: btop, bat, curl, delta, fd, fzf, jq, ripgrep, tree, wget.

## Files

| File | Purpose |
|------|---------|
| `flake.nix` | NixOS VM image definition (qcow2, both x86_64 and aarch64) |
| `agent-nixos.yaml` | Lima instance configuration (resources, mounts, provisioning) |
| `claude.json` | Claude Code project config provisioned into the guest |
| `nix/lima-init.nix` | NixOS module for Lima guest agent and mount setup |
| `nix/lima-init.sh` | Startup script that creates users, SSH keys, and fstab entries |
