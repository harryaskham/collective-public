# Windows devbox bootstrap

Provision a new Microsoft WSL devbox (`ms-dev-N`) from a fresh Windows host in
one command. The script installs WSL2 + NixOS-WSL, places the shared devbox SSH
key, clones the (private) `collective` flake over SSH, and runs the first
`cltv switch`. After that the box joins the tailnet non-interactively
(devbox-pool auth key) and converges its Windows host declaratively from inside
WSL and converges its Windows host declaratively from inside
WSL (Chrome, Slack, etc. via winget; power settings via `powercfg`).

## Windows convergence: headless vs admin (UAC)

The Windows devbox user is typically a **non-admin domain account** (e.g.
`REDMOND\you`). Windows convergence is therefore split into two halves:

- **Headless half (no UAC, automatic on every switch + timer):** user-scope
  winget installs (Slack, PowerShell, Windows Terminal, **Git via MinGit** —
  the portable zip variant) and per-user `powercfg` sleep/monitor timeouts.
  This runs entirely over SSH/systemd with no prompts.
- **Admin half (ONE interactive UAC consent):** machine-scope installs whose
  installers self-elevate (e.g. **Google Chrome**) and `powercfg /hibernate
  off`. These are bundled into a single command. After your first switch, run
  it once **inside the RDP / Cloud PC desktop's WSL terminal** and approve the
  single UAC prompt:

  ```bash
  devbox-windows-admin
  ```

  > **Important — must be run in the interactive RDP session, not over SSH.**
  > UAC uses the *secure desktop*. A prompt triggered from an SSH/systemd
  > context renders on session 0's invisible desktop and can never be clicked.
  > Run `devbox-windows-admin` (or `ps-sudo ...`) from the WSL terminal *inside*
  > the Cloud PC desktop (Windows 365 web client or the Windows App) so the
  > prompt paints in your live session where you can approve it. With
  > `ConsentPromptBehaviorUser=3` (non-admin domain default) you'll be asked
  > for credentials, not just Yes/No.

A non-admin user cannot avoid the UAC consent for the admin half; everything
else is fully automated.

## Waking the Cloud PC (Windows 365)

These devboxes are Windows 365 Cloud PCs. The WSL2 VM can be killed by host
memory pressure, and the Cloud PC itself deallocates when idle. If the node
shows `offline` on the tailnet and SSH times out, **reconnect to wake it**:

- **Windows App** (formerly Remote Desktop): open the `harryaskham` / Cloud PC
  entry, or
- **Web client:** `https://windows.cloud.microsoft/webclient/...` (authed as
  `harryaskham@microsoft.com`).

Connecting reboots/reattaches the Cloud PC; WSL restarts and rejoins the
tailnet within ~1 minute. (Hibernate-off + sleep-never, applied by the admin
and headless convergence, reduce — but on a Cloud PC do not fully eliminate —
idle deallocation.)

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
    opt into `devbox.windows` for declarative Windows app provisioning — put
    user-scope/portable packages in `winget.userPackages` (headless) and only
    machine-scope/self-elevating ones like `Google.Chrome` in
    `winget.adminPackages` (run `devbox-windows-admin` once for those))
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
