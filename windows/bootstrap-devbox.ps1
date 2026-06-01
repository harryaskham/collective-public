#Requires -Version 5.1
<#
.SYNOPSIS
  Bootstrap a new Microsoft WSL devbox (ms-dev-N) end to end from the Windows host.

.DESCRIPTION
  Installs WSL2 + NixOS-WSL, imports the NixOS distro, places the shared devbox
  SSH key, clones the (private) collective flake over SSH, and runs the first
  `cltv switch` for the given hostname. After this completes the box joins the
  tailnet non-interactively (devbox-pool auth key) and converges its Windows
  host declaratively from inside WSL.

  Designed to be run via:

    irm https://raw.githubusercontent.com/harryaskham/collective-public/main/windows/bootstrap-devbox.ps1 | iex

  When piped to iex you cannot pass parameters, so the script prompts
  interactively for everything it needs (hostname + SSH private key). To run
  non-interactively, download it first and invoke with -HostName / -KeyPath.

.PARAMETER HostName
  The collective machine name to switch to, e.g. ms-dev-2. Prompted if omitted.

.PARAMETER KeyPath
  Path to the shared devbox id_ed25519 private key on the Windows side. If
  omitted you are prompted to either give a path or paste the key contents.

.PARAMETER Distro
  WSL distro name to import the NixOS rootfs as. Defaults to NixOS.

.PARAMETER InstallRoot
  Directory on the Windows side to store the imported WSL disk. Defaults to
  $env:LOCALAPPDATA\WSL\<Distro>.

.PARAMETER NixOSWSLVersion
  NixOS-WSL release tag to import. Defaults to "latest".

.PARAMETER SkipWSLInstall
  Skip the `wsl --install` step (use if WSL2 is already present).
#>
[CmdletBinding()]
param(
  [string]$HostName,
  [string]$KeyPath,
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
function Die($m)   { Write-Host "[bootstrap] ERROR: $m" -ForegroundColor Red; exit 1 }

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
if (-not $HostName -or $HostName -eq "") {
  $HostName = Read-Host "Collective hostname for this devbox (e.g. ms-dev-2)"
}
if (-not $HostName) { Die "A hostname is required." }
if ($HostName -notmatch '^[a-zA-Z0-9._-]+$') { Die "Invalid hostname: $HostName" }

# The shared devbox id_ed25519 is both the host SSH identity AND the credential
# used to clone the private collective repo over SSH. Accept a path or pasted text.
$KeyMaterial = $null
if ($KeyPath -and (Test-Path $KeyPath)) {
  $KeyMaterial = Get-Content -Raw -Path $KeyPath
  Info "Read SSH key from $KeyPath"
} else {
  Write-Host ""
  Write-Host "Provide the shared devbox SSH private key (id_ed25519)." -ForegroundColor Cyan
  Write-Host "  1) Enter a path to the key file, or"           -ForegroundColor Cyan
  Write-Host "  2) Leave blank to paste the key contents."     -ForegroundColor Cyan
  $answer = Read-Host "Path to id_ed25519 (blank to paste)"
  if ($answer -and (Test-Path $answer)) {
    $KeyMaterial = Get-Content -Raw -Path $answer
    Info "Read SSH key from $answer"
  } elseif ($answer) {
    Die "Path not found: $answer"
  } else {
    Write-Host "Paste the private key now. Finish with a line containing only: END" -ForegroundColor Yellow
    $lines = @()
    while ($true) {
      $line = Read-Host
      if ($line -eq "END") { break }
      $lines += $line
    }
    $KeyMaterial = ($lines -join "`n") + "`n"
  }
}
if (-not $KeyMaterial -or $KeyMaterial.Trim() -eq "") { Die "No SSH key provided." }
if ($KeyMaterial -notmatch "BEGIN OPENSSH PRIVATE KEY") {
  Warn "Key does not look like an OpenSSH private key; continuing anyway."
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
  if ($NixOSWSLVersion -eq "latest") {
    $rel = Invoke-RestMethod "https://api.github.com/repos/nix-community/NixOS-WSL/releases/latest"
  } else {
    $rel = Invoke-RestMethod "https://api.github.com/repos/nix-community/NixOS-WSL/releases/tags/$NixOSWSLVersion"
  }
  $asset = $rel.assets | Where-Object { $_.name -eq "nixos.wsl" } | Select-Object -First 1
  if (-not $asset) { Die "Could not find nixos.wsl asset in release $($rel.tag_name)." }
  Info "NixOS-WSL $($rel.tag_name): $($asset.browser_download_url)"

  $tmp = Join-Path $env:TEMP "nixos.wsl"
  Info "Downloading rootfs to $tmp ..."
  Invoke-WebRequest -Uri $asset.browser_download_url -OutFile $tmp

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
$keyTmpWin = Join-Path $env:TEMP ("id_ed25519_" + [guid]::NewGuid().ToString("N").Substring(0,8))
# Write without BOM.
[System.IO.File]::WriteAllText($keyTmpWin, $KeyMaterialLF, (New-Object System.Text.UTF8Encoding($false)))

# Determine the default WSL user's home and copy the key into ~/.ssh/id_ed25519.
$keyTmpWsl = (wsl.exe -d $Distro -- wslpath -u "$keyTmpWin").Trim()
$installKey = @"
set -euo pipefail
HOME_DIR="`$HOME"
mkdir -p "`$HOME_DIR/.ssh"
chmod 700 "`$HOME_DIR/.ssh"
cp "$keyTmpWsl" "`$HOME_DIR/.ssh/id_ed25519"
chmod 600 "`$HOME_DIR/.ssh/id_ed25519"
# Derive the public key for convenience (matches the shared ms-dev key).
ssh-keygen -y -f "`$HOME_DIR/.ssh/id_ed25519" > "`$HOME_DIR/.ssh/id_ed25519.pub" 2>/dev/null || true
chmod 644 "`$HOME_DIR/.ssh/id_ed25519.pub" 2>/dev/null || true
# Pre-trust github.com so the clone is non-interactive.
ssh-keyscan -t ed25519,rsa github.com >> "`$HOME_DIR/.ssh/known_hosts" 2>/dev/null || true
echo "SSH key installed at `$HOME_DIR/.ssh/id_ed25519"
"@
wsl.exe -d $Distro -- bash -lc "$installKey" 2>&1 | Out-Host
Remove-Item -Force $keyTmpWin -ErrorAction SilentlyContinue
Ok "Shared devbox SSH key in place."

# ---------------------------------------------------------------------------
# 5. Clone the collective flake (private, over SSH) and run the first switch
# ---------------------------------------------------------------------------
Info "Cloning collective and running the first switch for '$HostName'..."
Info "This builds the system and may take a while on first run."

$switch = @"
set -euo pipefail
export PATH="/run/current-system/sw/bin:`$PATH"
cd "`$HOME"
if [ ! -d "`$HOME/collective/.git" ]; then
  GIT_SSH_COMMAND="ssh -i `$HOME/.ssh/id_ed25519 -o IdentitiesOnly=yes -o StrictHostKeyChecking=accept-new" \
    git clone git@github.com:harryaskham/collective.git "`$HOME/collective"
fi
cd "`$HOME/collective"
# First switch: use nixos-rebuild directly since cltv may not be on PATH yet.
sudo nixos-rebuild switch --flake ".#$HostName" --show-trace --print-build-logs --impure || {
  echo "First nixos-rebuild failed; you can re-run inside WSL with:";
  echo "  cd ~/collective && cltv switch";
  exit 1;
}
echo "First switch complete for $HostName."
"@
wsl.exe -d $Distro -- bash -lc "$switch" 2>&1 | Out-Host

Ok "Devbox '$HostName' bootstrapped."
Write-Host ""
Write-Host "Next steps:" -ForegroundColor Cyan
Write-Host "  - Open WSL:           wsl -d $Distro" -ForegroundColor Cyan
Write-Host "  - Re-switch anytime:  cd ~/collective && cltv switch" -ForegroundColor Cyan
Write-Host "  - Tailnet join + Windows convergence run automatically on switch." -ForegroundColor Cyan
