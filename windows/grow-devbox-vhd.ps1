# grow-devbox-vhd.ps1 — safely grow an existing WSL2 devbox VHD in place.
#
# The default operation raises the NixOS filesystem/VHD ceiling to 1536 GB and
# configures the WSL2 VM for 48 GB of RAM. It never unregisters, imports,
# recreates, or shrinks a distribution.

[CmdletBinding()]
param(
  [string]$Distro = "NixOS",
  [ValidatePattern('^\d+(KB|MB|GB|TB)$')]
  [string]$DiskSize = "1536GB",
  [ValidatePattern('^\d+(KB|MB|GB|TB)$')]
  [string]$Memory = "48GB",
  [string]$WslConfigPath = (Join-Path $env:USERPROFILE ".wslconfig"),
  [switch]$SkipWSLUpdate,
  [switch]$Force
)

$ErrorActionPreference = "Stop"
Set-StrictMode -Version Latest
$Utf8NoBom = New-Object System.Text.UTF8Encoding($false)

function Info($m) { Write-Host "[wsl-grow] $m" -ForegroundColor Cyan }
function Ok($m)   { Write-Host "[wsl-grow] $m" -ForegroundColor Green }
function Warn($m) { Write-Host "[wsl-grow] $m" -ForegroundColor Yellow }
function Die($m)  { throw "[wsl-grow] ERROR: $m" }

trap {
  Write-Host ""
  Write-Host $_.Exception.Message -ForegroundColor Red
  exit 1
}

function Set-IniValue {
  param(
    [string[]]$Lines,
    [string]$Section,
    [string]$Key,
    [string]$Value
  )

  $sectionPattern = '^\s*\[\s*' + [Regex]::Escape($Section) + '\s*\]\s*(?:[#;].*)?$'
  $keyPattern = '^\s*' + [Regex]::Escape($Key) + '\s*='
  $anySectionPattern = '^\s*\[[^]]+\]'
  $result = New-Object System.Collections.Generic.List[string]
  $sectionFound = $false
  $inTargetSection = $false
  $keyWritten = $false

  foreach ($line in $Lines) {
    if ($line -match $anySectionPattern) {
      if ($inTargetSection -and -not $keyWritten) {
        [void]$result.Add("$Key=$Value")
        $keyWritten = $true
      }

      $inTargetSection = ($line -match $sectionPattern)
      if ($inTargetSection) { $sectionFound = $true }
      [void]$result.Add($line)
      continue
    }

    if ($inTargetSection -and $line -match $keyPattern) {
      # Replace the first value and remove duplicate active definitions so the
      # resulting effective value is unambiguous. Comments remain untouched.
      if (-not $keyWritten) {
        [void]$result.Add("$Key=$Value")
        $keyWritten = $true
      }
      continue
    }

    [void]$result.Add($line)
  }

  if ($sectionFound) {
    if ($inTargetSection -and -not $keyWritten) {
      [void]$result.Add("$Key=$Value")
    }
  } else {
    if ($result.Count -gt 0 -and $result[$result.Count - 1] -ne "") {
      [void]$result.Add("")
    }
    [void]$result.Add("[$Section]")
    [void]$result.Add("$Key=$Value")
  }

  return $result.ToArray()
}

function Set-WslCapacityConfig {
  param(
    [string]$Path,
    [string]$MemoryValue,
    [string]$DiskValue
  )

  $lines = @()
  if (Test-Path -LiteralPath $Path) {
    # Auto-detect UTF BOMs so comments written by Windows PowerShell survive.
    $lines = @([IO.File]::ReadAllLines($Path))
  }
  $before = $lines -join "`n"

  $lines = @(Set-IniValue -Lines $lines -Section "wsl2" -Key "memory" -Value $MemoryValue)
  $lines = @(Set-IniValue -Lines $lines -Section "wsl2" -Key "defaultVhdSize" -Value $DiskValue)
  $after = $lines -join "`n"

  if ($before -eq $after) { return $false }

  $newline = [Environment]::NewLine
  [IO.File]::WriteAllText($Path, ($lines -join $newline) + $newline, $Utf8NoBom)
  return $true
}

function Convert-SizeToBytes {
  param([string]$Size)

  if ($Size -notmatch '^(\d+)(KB|MB|GB|TB)$') {
    Die "Invalid size '$Size'. Use an integer size such as 1536GB; WSL does not accept decimals."
  }

  $number = [UInt64]$Matches[1]
  $unit = $Matches[2].ToUpperInvariant()
  $multipliers = @{
    KB = [UInt64]1024
    MB = [UInt64](1024 * 1024)
    GB = [UInt64](1024 * 1024 * 1024)
    TB = [UInt64](1024L * 1024L * 1024L * 1024L)
  }
  return [UInt64]($number * $multipliers[$unit])
}

function Get-DistroStorageInfo {
  param([string]$Name)

  # The VHD/block device reaches the exact requested ceiling, but `df` reports
  # less because ext4 reserves space for metadata. Probe both so grow/shrink
  # decisions use the exact device size while messages show usable filesystem
  # capacity. `findmnt -v` omits source suffixes such as /dev/sdc[/]; strip one
  # explicitly as well for older util-linux versions.
  $probe = 'PATH=/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:/nix/var/nix/profiles/per-user/root/profile/bin:/etc/profiles/per-user/root/bin:$PATH; export PATH; findmnt_path=$(command -v findmnt 2>/dev/null); lsblk_path=$(command -v lsblk 2>/dev/null); df_path=$(command -v df 2>/dev/null); root_device=$(findmnt -vno SOURCE -T / 2>/dev/null); if [ -z "$root_device" ]; then root_device=$(findmnt -n -o SOURCE / 2>/dev/null); fi; root_device=${root_device%%\[*}; device_bytes=$(lsblk -b -n -o SIZE "$root_device" 2>/dev/null | head -n 1 | tr -d "[:space:]"); filesystem_bytes=$(df --block-size=1 --output=size / 2>/dev/null | tail -n 1 | tr -d "[:space:]"); printf "device=%s filesystem=%s source=%s findmnt=%s lsblk=%s df=%s\n" "$device_bytes" "$filesystem_bytes" "$root_device" "${findmnt_path:-missing}" "${lsblk_path:-missing}" "${df_path:-missing}"'
  $output = @(& wsl.exe -d $Name -u root -- sh -c $probe 2>$null)
  if ($LASTEXITCODE -ne 0) {
    Die "Could not read the '$Name' root block-device and filesystem sizes; refusing to resize without the grow-only safety check."
  }

  # Windows PowerShell 5.1 can expose redirected wsl.exe output with embedded
  # NULs. Parse labels from the joined, cleaned output instead of relying on
  # native line splitting or output encoding.
  $probeText = (($output -join " ") -replace "`0", "").Trim()
  $sizeMatch = [Regex]::Match($probeText, 'device=(?<device>\d+)\s+filesystem=(?<filesystem>\d+)')
  if (-not $sizeMatch.Success) {
    $diagnostic = ($probeText -replace '[\r\n]+', ' ').Trim()
    if (-not $diagnostic) { $diagnostic = "(empty)" }
    if ($diagnostic.Length -gt 300) { $diagnostic = $diagnostic.Substring(0, 300) + "..." }
    Die "Could not parse the '$Name' root block-device and filesystem sizes; refusing to resize without the grow-only safety check. Probe output: $diagnostic"
  }

  return [PSCustomObject]@{
    DeviceBytes = [UInt64]$sizeMatch.Groups["device"].Value
    FilesystemBytes = [UInt64]$sizeMatch.Groups["filesystem"].Value
  }
}

if (-not (Get-Command wsl.exe -ErrorAction SilentlyContinue)) {
  Die "wsl.exe is not available. Run this script in Windows PowerShell on the devbox host."
}

$distroNames = @(((& wsl.exe --list --quiet 2>$null) -replace "`0", "") | ForEach-Object { $_.Trim() } | Where-Object { $_ })
if ($LASTEXITCODE -ne 0) { Die "Could not list WSL distributions." }
if ($distroNames -notcontains $Distro) {
  Die "WSL distribution '$Distro' was not found. Available distributions: $($distroNames -join ', ')"
}

$verboseList = ((& wsl.exe --list --verbose 2>$null) -replace "`0", "") -join "`n"
if ($LASTEXITCODE -ne 0) { Die "Could not inspect WSL distribution versions." }
$distroPattern = '(?m)^\s*\*?\s*' + [Regex]::Escape($Distro) + '\s+.*\s+2\s*$'
if ($verboseList -notmatch $distroPattern) {
  Die "'$Distro' is not a WSL2 distribution; in-place VHD growth is only supported for WSL2."
}

$targetBytes = Convert-SizeToBytes $DiskSize
$currentStorage = Get-DistroStorageInfo $Distro
$currentDeviceBytes = [UInt64]$currentStorage.DeviceBytes
$currentFilesystemBytes = [UInt64]$currentStorage.FilesystemBytes
$currentDeviceGiB = [Math]::Round($currentDeviceBytes / 1GB, 1)
$currentFilesystemGiB = [Math]::Round($currentFilesystemBytes / 1GB, 1)
$targetGiB = [Math]::Round($targetBytes / 1GB, 1)

if ($targetBytes -lt $currentDeviceBytes) {
  Die "Refusing to shrink '$Distro': its root block device is already $currentDeviceGiB GiB, above the $targetGiB GiB target. No disk changes were made."
}

$configChanged = Set-WslCapacityConfig -Path $WslConfigPath -MemoryValue $Memory -DiskValue $DiskSize
if ($configChanged) {
  Ok "Merged memory=$Memory and defaultVhdSize=$DiskSize into $WslConfigPath (unrelated settings preserved)."
} else {
  Info "$WslConfigPath already has memory=$Memory and defaultVhdSize=$DiskSize."
}

if ($currentDeviceBytes -ge $targetBytes) {
  Ok "'$Distro' already has a $currentDeviceGiB GiB block device ($currentFilesystemGiB GiB ext4 filesystem); no VHD resize is needed."
  if ($configChanged) {
    Warn "The 48 GB memory cap will take effect after the next 'wsl --shutdown' and restart."
  }
  return
}

if (-not $SkipWSLUpdate) {
  Info "Updating WSL so the supported 'wsl --manage --resize' command is available (requires WSL 2.5+)..."
  # Keep native stderr separate. Windows PowerShell 5.1 turns redirected native
  # stderr (2>&1) into ErrorRecord objects, and ErrorActionPreference=Stop would
  # abort on e2fsck's informational version banner despite a successful resize.
  & wsl.exe --update
  if ($LASTEXITCODE -ne 0) {
    Warn "'wsl --update' failed; continuing in case WSL 2.5+ is already installed."
  }
}

Write-Host ""
Warn "Growing '$Distro' from a $currentDeviceGiB GiB block device ($currentFilesystemGiB GiB ext4 filesystem) to $DiskSize requires stopping all WSL distributions."
Warn "Close or save work in every WSL session first. The VHD will be expanded in place; it will not be recreated."
if (-not $Force) {
  $answer = Read-Host "Continue with the grow-only resize? (y/N)"
  if ($answer -notmatch '^[Yy]') { Die "Aborted before shutdown; no disk changes were made." }
}

Info "Stopping WSL before the in-place expansion..."
& wsl.exe --shutdown
if ($LASTEXITCODE -ne 0) { Die "'wsl --shutdown' failed; the resize was not attempted." }

Info "Growing '$Distro' to $DiskSize with the supported WSL VHD/filesystem resizer..."
& wsl.exe --manage $Distro --resize $DiskSize
if ($LASTEXITCODE -ne 0) {
  Die "WSL could not grow '$Distro'. Ensure WSL 2.5+ is installed with 'wsl --update'. The script did not unregister, import, recreate, or request a shrink."
}

$grownStorage = Get-DistroStorageInfo $Distro
$grownDeviceBytes = [UInt64]$grownStorage.DeviceBytes
$grownFilesystemBytes = [UInt64]$grownStorage.FilesystemBytes
$grownDeviceGiB = [Math]::Round($grownDeviceBytes / 1GB, 1)
$grownFilesystemGiB = [Math]::Round($grownFilesystemBytes / 1GB, 1)
if ($grownDeviceBytes -lt $targetBytes) {
  Die "WSL reported success, but the root block device is only $grownDeviceGiB GiB; expected $targetGiB GiB."
}

Ok "'$Distro' block device was grown in place from $currentDeviceGiB GiB to $grownDeviceGiB GiB."
Ok "The ext4 filesystem reports $grownFilesystemGiB GiB after filesystem metadata overhead."
Ok "WSL is configured for a $Memory memory cap and a $DiskSize default VHD ceiling."
