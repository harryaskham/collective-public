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

function Get-DistroFilesystemBytes {
  param([string]$Name)

  # Query the live root filesystem before taking WSL down. This is a safety
  # guard, not a capacity estimate: a target at or below the current filesystem
  # is refused so this script can never become a shrink path.
  $output = @(& wsl.exe -d $Name -u root -- df --block-size=1 --output=size / 2>$null)
  if ($LASTEXITCODE -ne 0) {
    Die "Could not read the '$Name' root filesystem size; refusing to resize without the grow-only safety check."
  }

  [UInt64]$size = 0
  foreach ($line in $output) {
    [UInt64]$candidate = 0
    if ([UInt64]::TryParse($line.Trim(), [ref]$candidate)) { $size = $candidate }
  }
  if ($size -eq 0) {
    Die "Could not parse the '$Name' root filesystem size; refusing to resize without the grow-only safety check."
  }
  return $size
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
$currentBytes = Get-DistroFilesystemBytes $Distro
$currentGiB = [Math]::Round($currentBytes / 1GB, 1)
$targetGiB = [Math]::Round($targetBytes / 1GB, 1)

# ext4 metadata makes df's filesystem size slightly smaller than the VHD's
# exact virtual size. Treat a filesystem within one percent as already at the
# requested ceiling, while refusing an actual lower target outright.
if ($targetBytes -lt $currentBytes) {
  Die "Refusing to shrink '$Distro': its root filesystem is already $currentGiB GiB, above the $targetGiB GiB target. No disk changes were made."
}

$configChanged = Set-WslCapacityConfig -Path $WslConfigPath -MemoryValue $Memory -DiskValue $DiskSize
if ($configChanged) {
  Ok "Merged memory=$Memory and defaultVhdSize=$DiskSize into $WslConfigPath (unrelated settings preserved)."
} else {
  Info "$WslConfigPath already has memory=$Memory and defaultVhdSize=$DiskSize."
}

$alreadySizedThreshold = [UInt64]([Math]::Floor($targetBytes * 0.99))
if ($currentBytes -ge $alreadySizedThreshold) {
  Ok "'$Distro' is already approximately $currentGiB GiB; no VHD resize is needed."
  if ($configChanged) {
    Warn "The 48 GB memory cap will take effect after the next 'wsl --shutdown' and restart."
  }
  return
}

if (-not $SkipWSLUpdate) {
  Info "Updating WSL so the supported 'wsl --manage --resize' command is available (requires WSL 2.5+)..."
  & wsl.exe --update 2>&1 | Out-Host
  if ($LASTEXITCODE -ne 0) {
    Warn "'wsl --update' failed; continuing in case WSL 2.5+ is already installed."
  }
}

Write-Host ""
Warn "Growing '$Distro' from about $currentGiB GiB to $DiskSize requires stopping all WSL distributions."
Warn "Close or save work in every WSL session first. The VHD will be expanded in place; it will not be recreated."
if (-not $Force) {
  $answer = Read-Host "Continue with the grow-only resize? (y/N)"
  if ($answer -notmatch '^[Yy]') { Die "Aborted before shutdown; no disk changes were made." }
}

Info "Stopping WSL before the in-place expansion..."
& wsl.exe --shutdown 2>&1 | Out-Host
if ($LASTEXITCODE -ne 0) { Die "'wsl --shutdown' failed; the resize was not attempted." }

Info "Growing '$Distro' to $DiskSize with the supported WSL VHD/filesystem resizer..."
& wsl.exe --manage $Distro --resize $DiskSize 2>&1 | Out-Host
if ($LASTEXITCODE -ne 0) {
  Die "WSL could not grow '$Distro'. Ensure WSL 2.5+ is installed with 'wsl --update'. The script did not unregister, import, recreate, or request a shrink."
}

$grownBytes = Get-DistroFilesystemBytes $Distro
$grownGiB = [Math]::Round($grownBytes / 1GB, 1)
if ($grownBytes -lt $alreadySizedThreshold) {
  Die "WSL reported success, but the root filesystem is only $grownGiB GiB; expected approximately $targetGiB GiB."
}

Ok "'$Distro' was grown in place from about $currentGiB GiB to $grownGiB GiB."
Ok "WSL is configured for a $Memory memory cap and a $DiskSize default VHD ceiling."
