# Windows devbox bootstrap

Provision a new Microsoft WSL devbox (`ms-dev-N`) from a fresh Windows host in
one command. The script installs WSL2 + NixOS-WSL, places the shared devbox SSH
key, clones the (private) `collective` flake over SSH, and runs the first
`cltv switch`. After that the box joins the tailnet non-interactively
(devbox-pool auth key) and converges its Windows host declaratively from inside
WSL (Chrome, Slack, etc. via winget; power settings via `powercfg`).

## One-liner (run in an elevated PowerShell on the Windows host)

```powershell
irm https://raw.githubusercontent.com/harryaskham/collective-public/main/windows/bootstrap-devbox.ps1 | iex
```

You will be prompted for:

1. **Hostname** — e.g. `ms-dev-2` (must already be registered in the flake).
2. **Shared devbox SSH key** — give a path to `id_ed25519`, or paste its
   contents (finish with a line containing only `END`). This single key is the
   host identity *and* the credential used to clone the private repo; all
   devbox instances reuse the `ms-dev` key so no sops re-keying is needed.

## Non-interactive

Download first, then pass parameters:

```powershell
$u = "https://raw.githubusercontent.com/harryaskham/collective-public/main/windows/bootstrap-devbox.ps1"
irm $u -OutFile bootstrap-devbox.ps1
./bootstrap-devbox.ps1 -HostName ms-dev-2 -KeyPath C:\path\to\id_ed25519
```

## Prerequisites

- A new `ms-dev-N` must be registered in `collective`:
  - `machines/ms-dev-N/nixos/configuration.nix` (copy `ms-dev-2` as a template;
    opt into `devbox.windows` for declarative Windows app provisioning)
  - `flake.nix`: `ms-dev-N = Tagged [VM WSL] System.NixOS.X86;`
  - `pkgs/collective-lib/ssh.nix`: duplicate the `ms-dev` pubkey + age key lines
    under `ms-dev-N` (shared key → identical age recipient → no sops changes)
- The `keys/ts/devbox-pool` Tailscale auth key (tag `devbox-pool`) must be in
  sops so the new node joins the tailnet automatically.

## Re-running

Safe to re-run: WSL install, distro import, key install, and clone are all
idempotent. To reimport the distro cleanly, first
`wsl --unregister NixOS`.

## WSL-side only (already at the "WSL imported + key placed" checkpoint)

If the NixOS-WSL distro is already imported and the shared key is already at
`~/.ssh/id_ed25519`, you can skip the Windows half entirely and run just the
clone + first switch from *inside* the distro:

```bash
# inside NixOS-WSL (wsl -d NixOS)
curl -fsSL https://raw.githubusercontent.com/harryaskham/collective-public/main/windows/devbox-switch.sh | bash -s -- ms-dev-2
```

`devbox-switch.sh` is the WSL-side half of the bootstrap (the same clone +
`nixos-rebuild switch` logic the PowerShell script runs over `wsl.exe`). It is
idempotent and resolves `$HOME` robustly on a fresh distro where the env var
may be empty.
