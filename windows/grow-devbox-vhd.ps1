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

  # Avoid `sh -c`: PowerShell 7's native argument handling can retokenize long
  # shell snippets differently across Windows builds. Call stable NixOS profile
  # paths directly, with no login-shell PATH or nested quoting dependency.
  $findmntPath = "/run/current-system/sw/bin/findmnt"
  $lsblkPath = "/run/current-system/sw/bin/lsblk"
  $dfPath = "/run/current-system/sw/bin/df"

  $sourceOutput = @(& wsl.exe -d $Name -u root -- $findmntPath -n -o SOURCE /)
  $sourceExitCode = $LASTEXITCODE
  $sourceText = (($sourceOutput -join " ") -replace "`0", "").Trim()
  $rootDevice = ($sourceText -replace '\[.*$', '').Trim()
  if ($sourceExitCode -ne 0 -or $rootDevice -notmatch '^/dev/\S+$') {
    if (-not $sourceText) { $sourceText = "(empty)" }
    Die "Could not identify the '$Name' root block device with $findmntPath (exit $sourceExitCode; output: $sourceText); refusing to resize without the grow-only safety check."
  }

  $deviceOutput = @(& wsl.exe -d $Name -u root -- $lsblkPath -b -n -o SIZE $rootDevice)
  $deviceExitCode = $LASTEXITCODE
  $deviceText = (($deviceOutput -join "`n") -replace "`0", "").Trim()
  $deviceMatch = [Regex]::Match($deviceText, '(?m)^\s*(?<bytes>\d+)\s*$')
  if ($deviceExitCode -ne 0 -or -not $deviceMatch.Success) {
    if (-not $deviceText) { $deviceText = "(empty)" }
    Die "Could not read the '$Name' root block-device size with $lsblkPath (exit $deviceExitCode; source: $rootDevice; output: $deviceText); refusing to resize without the grow-only safety check."
  }

  $filesystemOutput = @(& wsl.exe -d $Name -u root -- $dfPath --block-size=1 --output=size /)
  $filesystemExitCode = $LASTEXITCODE
  $filesystemText = (($filesystemOutput -join "`n") -replace "`0", "").Trim()
  $filesystemMatch = [Regex]::Match($filesystemText, '(?m)^\s*(?<bytes>\d+)\s*$')
  if ($filesystemExitCode -ne 0 -or -not $filesystemMatch.Success) {
    if (-not $filesystemText) { $filesystemText = "(empty)" }
    Die "Could not read the '$Name' root filesystem size with $dfPath (exit $filesystemExitCode; output: $filesystemText); refusing to resize without the grow-only safety check."
  }

  return [PSCustomObject]@{
    DeviceBytes = [UInt64]$deviceMatch.Groups["bytes"].Value
    FilesystemBytes = [UInt64]$filesystemMatch.Groups["bytes"].Value
  }
}

function Get-RunningDistroNames {
  $output = @(& wsl.exe --list --running --quiet)
  $exitCode = $LASTEXITCODE
  if ($exitCode -ne 0) {
    Die "Could not list running WSL distributions (exit $exitCode); refusing to assume the target is stopped."
  }
  return @(($output -replace "`0", "") | ForEach-Object { $_.Trim() } | Where-Object { $_ })
}

function Wait-DistroStopped {
  param(
    [string]$Name,
    [int]$TimeoutSeconds = 45,
    [int]$StableSeconds = 3
  )

  $deadline = (Get-Date).AddSeconds($TimeoutSeconds)
  $stoppedSince = $null
  while ((Get-Date) -lt $deadline) {
    $runningDistros = @(Get-RunningDistroNames)
    if ($runningDistros -notcontains $Name) {
      if ($null -eq $stoppedSince) { $stoppedSince = Get-Date }
      if (((Get-Date) - $stoppedSince).TotalSeconds -ge $StableSeconds) { return $true }
    } else {
      # A background task restarted the distro; require a new uninterrupted
      # stopped interval before allowing the VHD operation.
      $stoppedSince = $null
    }
    Start-Sleep -Milliseconds 500
  }
  return $false
}

function Stop-WslForResize {
  param([string]$Name)

  # The read-only safety probe starts the distro. Explicitly terminate that
  # distro, shut down the WSL VM, then wait for a stable stopped interval so the
  # VHD handle is released before `wsl --manage` runs.
  Info "Terminating '$Name' and stopping WSL before the in-place expansion..."
  & wsl.exe --terminate $Name
  $terminateExitCode = $LASTEXITCODE
  if ($terminateExitCode -ne 0) {
    Warn "Targeted terminate returned exit $terminateExitCode; continuing with a full WSL shutdown."
  }

  & wsl.exe --shutdown
  if ($LASTEXITCODE -ne 0) { Die "'wsl --shutdown' failed; the resize was not attempted." }

  Info "Waiting for '$Name' to remain stopped and release its VHD..."
  if (-not (Wait-DistroStopped -Name $Name)) {
    Die "'$Name' did not remain stopped for 3 seconds within the 45-second timeout. A background task may be restarting it; the resize was not attempted."
  }
  Ok "'$Name' is stopped and its VHD is ready for an offline grow."
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

Stop-WslForResize -Name $Distro

Info "Growing '$Distro' to $DiskSize with the supported WSL VHD/filesystem resizer..."
& wsl.exe --manage $Distro --resize $DiskSize
$resizeExitCode = $LASTEXITCODE
if ($resizeExitCode -ne 0) {
  # A scheduled task can restart WSL in the small gap between the stopped-state
  # barrier and `--manage`. Retry only when WSL confirms the target is running;
  # other failures remain fail-closed and are never retried blindly.
  $runningAfterFailure = @(Get-RunningDistroNames)
  if ($runningAfterFailure -contains $Distro) {
    Warn "'$Distro' restarted before the resize command; stopping it and retrying once."
    Stop-WslForResize -Name $Distro
    & wsl.exe --manage $Distro --resize $DiskSize
    $resizeExitCode = $LASTEXITCODE
  }
}
if ($resizeExitCode -ne 0) {
  Die "WSL could not grow '$Distro' (exit $resizeExitCode). Ensure WSL 2.5+ is installed and no background task is restarting the distro. The script did not unregister, import, recreate, or request a shrink."
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
