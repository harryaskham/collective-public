# bootstrap-devbox.ps1 — provision a Microsoft WSL devbox (ms-dev-N) end to end.
#
# ONE idempotent command: run it to bring up a FRESH box, or RE-RUN it to
# continue a partial/interrupted bringup to completion. It skips WSL install +
# rootfs import when the distro already exists, re-places the key, and delegates
# the switch to the canonical (also-resumable) devbox-switch.sh.
#
# RECOMMENDED (supports a key file, keeps the window open on error):
#   $u = "https://raw.githubusercontent.com/harryaskham/collective-public/main/windows/bootstrap-devbox.ps1"
#   irm $u -OutFile "$env:TEMP\bootstrap-devbox.ps1"
#   & "$env:TEMP\bootstrap-devbox.ps1" -DevboxHost ms-dev-2 -KeyPath C:\path\to\id_ed25519
#
# QUICK (fully interactive; prompts for hostname + key):
#   & ([scriptblock]::Create((irm "$u"))) -DevboxHost ms-dev-2
#
# Note: do NOT run as a bare `irm $u | iex` if you need to pass parameters —
# iex cannot bind params. The scriptblock form above can.

[CmdletBinding()]
param(
  [string]$DevboxHost,
  [string]$KeyPath,
  [switch]$KeyFromClipboard,
  [string]$Distro = "NixOS",
  [string]$InstallRoot,
  [string]$NixOSWSLVersion = "latest",
  [switch]$SkipWSLInstall
)

$ErrorActionPreference = "Stop"
Set-StrictMode -Version Latest

function Info($m)  { Write-Host "[bootstrap] $m" -ForegroundColor Cyan }
function Ok($m)    { Write-Host "[bootstrap] $m" -ForegroundColor Green }
function Warn($m)  { Write-Host "[bootstrap] $m" -ForegroundColor Yellow }
# Die throws instead of `exit` so that, when this script is run via `irm | iex`
# in an interactive window, a failure does not silently close the host window.
function Die($m)   { throw "[bootstrap] ERROR: $m" }

# Surface any terminating error (including Die) and keep the window open so the
# message is visible. When piped to iex, `exit` would close the whole window.
trap {
  Write-Host ""
  Write-Host "[bootstrap] FAILED: $($_.Exception.Message)" -ForegroundColor Red
  Write-Host "[bootstrap] $($_.ScriptStackTrace)" -ForegroundColor DarkGray
  Write-Host ""
  try { Read-Host "Press Enter to close" | Out-Null } catch { }
  break
}

# ---------------------------------------------------------------------------
# 0. Preconditions
# ---------------------------------------------------------------------------
$isAdmin = ([Security.Principal.WindowsPrincipal] `
  [Security.Principal.WindowsIdentity]::GetCurrent() `
  ).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
if (-not $isAdmin) {
  Warn "Not running as Administrator. WSL install may prompt for elevation."
}

if (-not $InstallRoot -or $InstallRoot -eq "") {
  $InstallRoot = Join-Path $env:LOCALAPPDATA "WSL\$Distro"
}

# ---------------------------------------------------------------------------
# 1. Gather inputs (interactive-friendly for `irm | iex`)
# ---------------------------------------------------------------------------
if (-not $DevboxHost -or $DevboxHost -eq "") {
  $DevboxHost = Read-Host "Collective hostname for this devbox (e.g. ms-dev-2)"
}
if (-not $DevboxHost) { Die "A hostname is required." }
if ($DevboxHost -notmatch '^[a-zA-Z0-9._-]+$') { Die "Invalid hostname: $DevboxHost" }

# The shared devbox id_ed25519 is both the host SSH identity AND the credential
# used to clone the private collective repo over SSH. Accept a path, the
# clipboard (most reliable for multi-line keys over RDP), or line-by-line paste.
$KeyMaterial = $null
if ($KeyPath -and (Test-Path $KeyPath)) {
  $KeyMaterial = Get-Content -Raw -Path $KeyPath
  Info "Read SSH key from $KeyPath"
} elseif ($KeyFromClipboard) {
  $KeyMaterial = Get-Clipboard -Raw
  Info "Read SSH key from clipboard"
} else {
  Write-Host ""
  Write-Host "Provide the shared devbox SSH private key (id_ed25519)." -ForegroundColor Cyan
  Write-Host "  1) Enter a path to the key file (most reliable), or"  -ForegroundColor Cyan
  Write-Host "  2) Type 'clip' to read it from the clipboard, or"       -ForegroundColor Cyan
  Write-Host "  3) Leave blank to paste line-by-line."                  -ForegroundColor Cyan
  $answer = Read-Host "Path / 'clip' / blank"
  if ($answer -and (Test-Path $answer)) {
    $KeyMaterial = Get-Content -Raw -Path $answer
    Info "Read SSH key from $answer"
  } elseif ($answer -eq "clip") {
    $KeyMaterial = Get-Clipboard -Raw
    Info "Read SSH key from clipboard"
  } elseif ($answer) {
    Die "Path not found: $answer"
  } else {
    Write-Host "Paste the private key now (right-click to paste in this window)." -ForegroundColor Yellow
    Write-Host "Finish with a line containing only: END" -ForegroundColor Yellow
    $lines = @()
    while ($true) {
      $line = Read-Host
      # Tolerate a pasted END with stray whitespace/CR.
      if ($line.Trim() -eq "END") { break }
      $lines += ($line -replace "`r", "")
    }
    $KeyMaterial = ($lines -join "`n").TrimEnd() + "`n"
  }
}
if (-not $KeyMaterial -or $KeyMaterial.Trim() -eq "") { Die "No SSH key provided." }
# Normalize to LF and ensure a trailing newline; OpenSSH rejects CRLF keys.
$KeyMaterial = ($KeyMaterial -replace "`r`n", "`n" -replace "`r", "`n").TrimEnd() + "`n"
if ($KeyMaterial -notmatch "BEGIN OPENSSH PRIVATE KEY" -and $KeyMaterial -notmatch "BEGIN .*PRIVATE KEY") {
  Warn "Pasted text does not contain a PRIVATE KEY header; the clone step will likely fail."
  $cont = Read-Host "Continue anyway? (y/N)"
  if ($cont -notmatch '^[Yy]') { Die "Aborted: no valid private key provided." }
}

# ---------------------------------------------------------------------------
# 2. Install WSL2
# ---------------------------------------------------------------------------
if (-not $SkipWSLInstall) {
  Info "Ensuring WSL2 is installed..."
  try {
    wsl.exe --install --no-distribution 2>&1 | Out-Host
  } catch {
    Warn "wsl --install --no-distribution failed or already satisfied: $_"
  }
  try { wsl.exe --set-default-version 2 2>&1 | Out-Host } catch { Warn "set-default-version 2 failed: $_" }
  try { wsl.exe --update 2>&1 | Out-Host } catch { Warn "wsl --update failed: $_" }
} else {
  Info "Skipping WSL install (per -SkipWSLInstall)."
}

# ---------------------------------------------------------------------------
# 3. Download + import the NixOS-WSL rootfs
# ---------------------------------------------------------------------------
$existing = (wsl.exe --list --quiet 2>$null) -replace "`0", ""
if ($existing -match [Regex]::Escape($Distro)) {
  Warn "WSL distro '$Distro' already exists; skipping import. Unregister it first to reimport."
} else {
  Info "Resolving NixOS-WSL release ($NixOSWSLVersion)..."
  # GitHub's REST API rejects requests without a User-Agent header (HTTP 403).
  # Force TLS 1.2 and send a UA; fall back to the redirect-resolving
  # /releases/latest/download URL if the API is rate-limited.
  try { [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocol]::Tls12 } catch {}
  $ghHeaders = @{ "User-Agent" = "collective-devbox-bootstrap"; "Accept" = "application/vnd.github+json" }
  $asset = $null
  try {
    if ($NixOSWSLVersion -eq "latest") {
      $rel = Invoke-RestMethod "https://api.github.com/repos/nix-community/NixOS-WSL/releases/latest" -Headers $ghHeaders
    } else {
      $rel = Invoke-RestMethod "https://api.github.com/repos/nix-community/NixOS-WSL/releases/tags/$NixOSWSLVersion" -Headers $ghHeaders
    }
    $asset = $rel.assets | Where-Object { $_.name -eq "nixos.wsl" } | Select-Object -First 1
    if ($asset) { $downloadUrl = $asset.browser_download_url; $relTag = $rel.tag_name }
  } catch {
    Warn "GitHub API lookup failed ($_); falling back to releases/latest/download redirect."
  }
  if (-not $asset) {
    # API-free fallback: GitHub serves the asset by name under /releases/latest/download/.
    if ($NixOSWSLVersion -eq "latest") {
      $downloadUrl = "https://github.com/nix-community/NixOS-WSL/releases/latest/download/nixos.wsl"
    } else {
      $downloadUrl = "https://github.com/nix-community/NixOS-WSL/releases/download/$NixOSWSLVersion/nixos.wsl"
    }
    $relTag = $NixOSWSLVersion
  }
  Info "NixOS-WSL $relTag : $downloadUrl"

  $tmp = Join-Path $env:TEMP "nixos.wsl"
  Info "Downloading rootfs to $tmp ..."
  Invoke-WebRequest -Uri $downloadUrl -OutFile $tmp -Headers $ghHeaders

  New-Item -ItemType Directory -Force -Path $InstallRoot | Out-Null
  Info "Importing as WSL distro '$Distro' into $InstallRoot ..."
  # Modern NixOS-WSL ships a .wsl bundle: import via --from-file for tar import.
  try {
    wsl.exe --import $Distro $InstallRoot $tmp --version 2 2>&1 | Out-Host
  } catch {
    Warn "wsl --import failed, retrying with --import-in-place style: $_"
    wsl.exe --install --from-file $tmp 2>&1 | Out-Host
  }
  wsl.exe --set-default $Distro 2>&1 | Out-Host
  Ok "NixOS-WSL imported as '$Distro'."
}

# ---------------------------------------------------------------------------
# 4. Place the shared SSH key inside WSL
# ---------------------------------------------------------------------------
Info "Installing the devbox SSH key inside WSL..."
# Normalize CRLF -> LF; ssh refuses keys with carriage returns.
$KeyMaterialLF = $KeyMaterial -replace "`r`n", "`n" -replace "`r", "`n"
# Avoid all Windows<->WSL path translation (wslpath mangles backslashes when a
# Windows path is passed through wsl.exe). Instead, base64-encode the key and
# pipe it into the distro over stdin, decoding inside WSL.
$keyB64 = [Convert]::ToBase64String([System.Text.Encoding]::UTF8.GetBytes($KeyMaterialLF))
$installKey = @"
set -euo pipefail
# A freshly imported NixOS-WSL has no default user yet, so `$HOME may be empty
# in this non-login `bash -lc` context. Resolve the real home directory and
# export HOME so all downstream tools (ssh, git, nixos-rebuild) agree on it.
HOME_DIR="`$(eval echo ~`$(id -un) 2>/dev/null)"
if [ -z "`$HOME_DIR" ] || [ "`$HOME_DIR" = "~`$(id -un)" ]; then HOME_DIR="`$(getent passwd `$(id -un) | cut -d: -f6)"; fi
if [ -z "`$HOME_DIR" ]; then HOME_DIR="/root"; fi
export HOME="`$HOME_DIR"
mkdir -p "`$HOME_DIR/.ssh"
chmod 700 "`$HOME_DIR/.ssh"
# The base64 of the key is passed as the first argument.
printf '%s' "`$1" | base64 -d > "`$HOME_DIR/.ssh/id_ed25519"
chmod 600 "`$HOME_DIR/.ssh/id_ed25519"
# Derive the public key for convenience (matches the shared ms-dev key).
ssh-keygen -y -f "`$HOME_DIR/.ssh/id_ed25519" > "`$HOME_DIR/.ssh/id_ed25519.pub" 2>/dev/null || true
chmod 644 "`$HOME_DIR/.ssh/id_ed25519.pub" 2>/dev/null || true
# Pre-trust github.com so the clone is non-interactive.
ssh-keyscan -t ed25519,rsa github.com >> "`$HOME_DIR/.ssh/known_hosts" 2>/dev/null || true
echo "SSH key installed at `$HOME_DIR/.ssh/id_ed25519"
"@
wsl.exe -d $Distro -- bash -lc ($installKey -replace "`r","") "_" "$keyB64" 2>&1 | Out-Host
Ok "Shared devbox SSH key in place."

# ---------------------------------------------------------------------------
# 5. Run the canonical, idempotent switch (devbox-switch.sh) inside WSL
# ---------------------------------------------------------------------------
# Single source of truth: fetch devbox-switch.sh and run it rather than carrying
# a divergent inline switch here. devbox-switch.sh handles clone, corp-key
# materialization, the flake archive (with a real nix git, not Windows git.exe),
# the first switch, and the place-key-for-user + reactivate recovery — and it is
# idempotent/resumable, so re-running this whole bootstrap continues a partial
# bringup to completion.
Info "Fetching the canonical devbox-switch.sh and running the switch for '$DevboxHost'..."
Info "This clones the collective flake, materializes the corp key, builds the system, and completes activation. May take a while on first run."

$switchUrl = "https://raw.githubusercontent.com/harryaskham/collective-public/main/windows/devbox-switch.sh"
try { [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocol]::Tls12 } catch {}
$switchScript = Invoke-RestMethod -Uri $switchUrl -Headers @{ "User-Agent" = "collective-devbox-bootstrap" }
# LF-normalize + base64 so no CRLF or Windows<->WSL path translation reaches WSL.
$switchScript = ($switchScript -replace "`r`n","`n" -replace "`r","`n")
$switchB64 = [Convert]::ToBase64String([System.Text.Encoding]::UTF8.GetBytes($switchScript))
$runSwitch = @"
set -euo pipefail
printf '%s' "`$1" | base64 -d > /tmp/devbox-switch.sh
bash /tmp/devbox-switch.sh "$DevboxHost"
"@
wsl.exe -d $Distro -- bash -lc ($runSwitch -replace "`r","") "_" "$switchB64" 2>&1 | Out-Host

Ok "Devbox '$DevboxHost' bootstrapped."
Write-Host ""
Write-Host "Next steps:" -ForegroundColor Cyan
Write-Host "  - Open WSL:           wsl -d $Distro" -ForegroundColor Cyan
Write-Host "  - Re-switch anytime:  cd ~/collective && cltv switch" -ForegroundColor Cyan
Write-Host "  - Tailnet join + Windows convergence run automatically on switch." -ForegroundColor Cyan
