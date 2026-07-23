# Windows devbox bootstrap

Provision a new Microsoft WSL devbox (`ms-dev-N`) from a fresh Windows host in
one command. The script installs WSL2 + NixOS-WSL, places the shared devbox SSH
key, clones the (private) `collective` flake over SSH, and runs the first
`cltv switch`. After that the box joins the tailnet non-interactively
(devbox-pool auth key) and converges its Windows host declaratively from inside
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

### Preventing idle lock and session eviction

Sleep/hibernate and workstation locking are separate Windows policy paths. The
current convergence handles both:

- the interactive user gets a Startup keepalive that holds
  `SetThreadExecutionState` and emits real input every ~45 seconds (the older
  cursor-position change did not update Windows' idle clock);
- user screensaver and lock policy hives are disabled;
- `devbox-windows-admin` disables machine inactivity, lock-on-wake, lock-screen,
  and RDS idle/disconnected-session limits;
- the `DevboxKeepAwake` SYSTEM task applies those settings immediately, at boot,
  and every two minutes so Group Policy/MDM refresh cannot silently restore
  them.

After updating an existing devbox, apply both halves once from its interactive
RDP/Cloud PC WSL terminal:

```bash
cd ~/collective
git pull --ff-only
cltv switch
devbox-windows-admin   # approve the single UAC prompt
```

The interactive keepalive writes
`%LOCALAPPDATA%\devbox\keepactive.log`. If the machine still locks, inspect that
log and the effective `DeviceLock/MaxInactivityTimeDeviceLock` Intune policy;
an enforced central compliance policy may need to be removed in Intune rather
than only converged locally.

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

## Before bootstrap: register the machine and its exact key

Bootstrap assumes the target already evaluates in `collective`. Complete all of
these first:

1. Add `machines/ms-dev-N/nixos/configuration.nix` (copy an existing devbox and
   set `devbox.hostName`).
2. Add `ms-dev-N = Tagged [VM WSL] System.NixOS.X86;` to `flake.nix`.
3. Add **both** `authorizedKeys.ms-dev-N` and
   `precomputedAgeKeys.ms-dev-N` in `pkgs/collective-lib/ssh.nix`. The values
   must match the private key that will be supplied to bootstrap. If reusing
   another machine's key (for example `ms-mac`), copy that machine's public SSH
   and precomputed age values; if reusing the shared devbox key, copy `ms-dev`.
4. Add `keys/nix/attic/caches/ms-dev-N` in
   `standalone/secrets/secrets.yaml`. Because the devbox profile enables
   `atticd`, `server_env` is a hard evaluation/build dependency; add the
   corresponding `push` and `public` values as well for cache administration.
5. Ensure the reused/new age recipient can decrypt `secrets.yaml`. Reusing an
   already-enrolled key such as `ms-mac` needs no new recipient; a genuinely new
   key requires re-keying the sops file.
6. Ensure the shared `keys/cacophony/ts/devbox-pool` Tailscale auth key is
   present in sops.

Commit and push these changes before running bootstrap. Missing `flake.nix`,
SSH/age, or per-host Attic entries cannot be repaired by the Windows installer.

## One-liner (run in an elevated PowerShell on the Windows host)

```powershell
$h=@{"User-Agent"="collective-devbox-bootstrap";"Cache-Control"="no-cache"}; $r="https://api.github.com/repos/harryaskham/collective-public/git/ref/heads/main"; $s=(irm "${r}?nocache=$([guid]::NewGuid())" -Headers $h).object.sha; & ([scriptblock]::Create((irm "https://raw.githubusercontent.com/harryaskham/collective-public/$s/windows/bootstrap-devbox.ps1" -Headers $h))) -PublicCommit $s
```

This resolves `main` through GitHub's API and fetches the bootstrap by immutable
commit SHA; a raw branch URL can remain stale despite a query-string cache
buster.

You will be prompted for:

1. **Hostname** — e.g. `ms-dev-2` (must already be registered in the flake).
2. **Registered SSH key** — give a path to `id_ed25519`, or paste its contents
   (finish with a line containing only `END`). This single key is the host
   identity and the credential used to clone the private repo; its public and
   age values must match the pre-bootstrap `ssh.nix` entries above.

### Resume after an interrupted or failed bootstrap

If WSL/NixOS was already installed before the bootstrap stopped, **do not
unregister or reinstall the distro**. Run this in elevated PowerShell:

```powershell
$h=@{"User-Agent"="collective-devbox-bootstrap";"Cache-Control"="no-cache"}; $r="https://api.github.com/repos/harryaskham/collective-public/git/ref/heads/main"; $s=(irm "${r}?nocache=$([guid]::NewGuid())" -Headers $h).object.sha; $p=Join-Path $env:TEMP "bootstrap-devbox.ps1"; irm "https://raw.githubusercontent.com/harryaskham/collective-public/$s/windows/bootstrap-devbox.ps1" -Headers $h -OutFile $p; & $p -PublicCommit $s -SkipWSLInstall
```

This downloads the latest script without using a cached copy, detects and
preserves the existing `NixOS` distro, then resumes the WSL-side switch. The
hostname is persisted at `%USERPROFILE%\collective-bootstrap-name.txt`; when a
valid `/root/.ssh/id_ed25519` already exists in WSL, it is reused without
copying the private key back into Windows or prompting again. An older run that
predates the state file may require `-DevboxHost ms-dev-N` once.

During setup/resume, root's key material seeds the configured user's `.ssh`
(private/public key, derived `.age`, and available corp key), and a bootstrap
`/root/collective` checkout is moved to `~/collective` with user ownership. For
the older `ΓÇÿ/.sshΓÇÖ` error, the special characters were only PowerShell 5.1
decoding GNU's UTF-8 quotes with an OEM code page; the real path was `/.ssh`.

By default, accepted flake `nixConfig` is authoritative for substituters and
trusted keys (including a pre-tailnet Funnel/Attic cache). Only add
`-UseDefaultSubs` to opt into the script's fallback public cache list.

## Non-interactive

Download first, then pass parameters:

```powershell
$h = @{ "User-Agent" = "collective-devbox-bootstrap"; "Cache-Control" = "no-cache" }
$r = "https://api.github.com/repos/harryaskham/collective-public/git/ref/heads/main"
$s = (irm "${r}?nocache=$([guid]::NewGuid())" -Headers $h).object.sha
$u = "https://raw.githubusercontent.com/harryaskham/collective-public/$s/windows/bootstrap-devbox.ps1"
irm $u -Headers $h -OutFile bootstrap-devbox.ps1
./bootstrap-devbox.ps1 -PublicCommit $s -DevboxHost ms-dev-2 -KeyPath C:\path\to\id_ed25519
```

## Re-running

Safe to re-run: WSL install, distro import, key install, and clone are all
idempotent. To intentionally discard and reimport the distro cleanly, first run
`wsl --unregister NixOS`; this is not required for normal recovery.

## WSL-side only (already at the "WSL imported + key placed" checkpoint)

If the NixOS-WSL distro is already imported and the shared key is already at
`~/.ssh/id_ed25519`, you can skip the Windows half entirely and run just the
clone + first switch from *inside* the distro:

```bash
# Existing root checkout (best resume path; no hostname/key prompts):
cd /root/collective
git pull --ff-only
bash collective-public/windows/devbox-switch.sh ms-dev-2

# Or, when no checkout exists yet, download before executing. Do not curl|bash:
curl -fsSLo /tmp/devbox-switch.sh \
  https://raw.githubusercontent.com/harryaskham/collective-public/main/windows/devbox-switch.sh
bash /tmp/devbox-switch.sh ms-dev-2
```

`devbox-switch.sh` is the WSL-side half of the bootstrap (the same clone +
`nixos-rebuild switch` logic the PowerShell script runs over `wsl.exe`). It is
idempotent, resolves `$HOME` robustly, reuses root bootstrap material, and moves
the checkout into the configured user's home. It must execute from a complete
file rather than a pipe because Nix/systemctl may read stdin. Pass
`--use-default-subs` only to opt into fallback public caches; otherwise the
accepted flake cache configuration is used.
