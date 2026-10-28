# devbox-maintenance.ps1 - agent-friendly offline maintenance for a WSL2 devbox.
#
# Some maintenance needs a distro (and with it SSH, agents and builds) to be
# stopped, so it cannot be driven from inside that distro. This script runs on
# the Windows host:
#
#   -Install    Register the elevated DevboxMaintenance task (one UAC consent).
#               The script is copied to an Administrators-only directory, so an
#               unelevated process can queue requests but not change the code
#               the task runs.
#   -FromTask   What the task runs: take the queued request, stop the target
#               distro (shutting all of WSL down when the VHD must be released,
#               with the WSL watchdog paused), do the maintenance, then restart
#               every distro that was running and resume the watchdog.
#               Restoration also runs when maintenance fails.
#   (default)   The same operation, run directly from an elevated console.
#   -DryRun     Report what would happen. Safe while WSL runs; no elevation.
#
# Operations:
#   Compact  fstrim inside the distro, then compact its VHDX offline
#            (Optimize-VHD, else DiskPart); optionally mark it sparse. WSL
#            keeps every distro's VHD attached while its VM runs, so Compact
#            always shuts WSL down briefly, devbox distro included.
#   Cycle    Stop and restart only (canary for the stop/restore path).
#   Move     Move the distro's VHD to <Drive>:\WSL\<Distro> on another fixed
#            drive (wsl --manage --move), for example from a 512 GB C: to a
#            1.5 TB Q:. A request names only the drive letter, never a path.
#            Needs no elevation; like Compact it shuts WSL down.
#
# Every run writes a JSON receipt and a transcript under
# %LOCALAPPDATA%\devbox\maintenance, readable from WSL afterwards. The script
# never unregisters, imports, resizes or recreates a distribution, moves one
# only with Move, and takes the VHDX path from WSL's own registration, never
# from a request.

[CmdletBinding(DefaultParameterSetName = "Direct")]
param(
  [Parameter(ParameterSetName = "Install", Mandatory = $true)]
  [switch]$Install,
  # Install only: the user the task runs as (default: the current user). Admin
  # convergence passes the unelevated user, since UAC may elevate another admin.
  [Parameter(ParameterSetName = "Install")]
  [string]$TaskUser = "",
  [Parameter(ParameterSetName = "Task", Mandatory = $true)]
  [switch]$FromTask,
  [Parameter(ParameterSetName = "Direct")]
  [ValidateSet("Compact", "Cycle", "Move")]
  [string]$Operation = "Compact",
  # Move only: the fixed drive to move to (a letter such as Q).
  [Parameter(ParameterSetName = "Direct")]
  [ValidatePattern('^[A-Za-z]?$')]
  [string]$TargetDrive = "",
  [Parameter(ParameterSetName = "Direct")]
  [string]$Distro = "NixOS",
  [Parameter(ParameterSetName = "Direct")]
  [switch]$SetSparse,
  [Parameter(ParameterSetName = "Direct")]
  [switch]$SkipTrim,
  [Parameter(ParameterSetName = "Direct")]
  [switch]$VerifyBoot,
  [Parameter(ParameterSetName = "Direct")]
  [switch]$DryRun,
  [Parameter(ParameterSetName = "Direct")]
  [switch]$Force,
  [string]$StateDir = (Join-Path $env:LOCALAPPDATA "devbox\maintenance"),
  # Tests dot-source the functions without running anything.
  [switch]$NoMain
)

$ErrorActionPreference = "Stop"
Set-StrictMode -Version Latest

$script:Mode = $PSCmdlet.ParameterSetName
$script:TaskName = "DevboxMaintenance"
$script:InstallDir = Join-Path $env:ProgramData "Collective\DevboxMaintenance"
$script:InstalledScript = Join-Path $script:InstallDir "devbox-maintenance.ps1"
$script:WatchdogDistro = "NixOS"
$script:PausePath = Join-Path $env:LOCALAPPDATA "devbox\wsl-watchdog.pause"
$script:LxssRoot = "HKCU:\Software\Microsoft\Windows\CurrentVersion\Lxss"
$script:Operations = @("Compact", "Cycle", "Move")
$script:CreatedPause = $false
$script:Receipt = $null
$script:ReceiptPath = $null
$script:Utf8NoBom = New-Object System.Text.UTF8Encoding($false)

function Write-Line {
  param([string]$Level, [string]$Message)
  $colors = @{ info = "Cyan"; ok = "Green"; warn = "Yellow"; error = "Red" }
  Write-Host "[devbox-maintenance] $Message" -ForegroundColor $colors[$Level]
}

function Test-Administrator {
  $identity = [Security.Principal.WindowsIdentity]::GetCurrent()
  $principal = New-Object Security.Principal.WindowsPrincipal($identity)
  return $principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
}

function Get-Field {
  param($Object, [string]$Name, $Default)
  if ($null -ne $Object -and $Object.PSObject.Properties[$Name]) { return $Object.$Name }
  return $Default
}

# Validate a queued request. Only whitelisted operations, a plain distro name
# and boolean flags pass; anything else is rejected before WSL is touched.
function ConvertTo-MaintenanceRequest {
  param($Object)
  if ($null -eq $Object) { throw "Request is empty." }
  $id = [string](Get-Field $Object "id" "")
  if ($id -notmatch '^[A-Za-z0-9-]{8,64}$') { throw "Request id '$id' is invalid." }
  $operation = [string](Get-Field $Object "operation" "")
  if ($script:Operations -notcontains $operation) {
    throw "Operation '$operation' is not one of: $($script:Operations -join ', ')."
  }
  $distroName = [string](Get-Field $Object "distro" "")
  if ($distroName -notmatch '^[A-Za-z0-9._-]{1,64}$' -or $distroName -match '^\.+$') { throw "Distro name '$distroName' is invalid." }
  # Move takes a drive letter only; the layout under it is fixed.
  $drive = [string](Get-Field $Object "targetDrive" "")
  if ($operation -eq "Move") {
    if ($drive -notmatch '^[A-Za-z]$') { throw "Move needs a target drive letter (for example Q), not '$drive'." }
    $drive = $drive.ToUpperInvariant()
  } elseif ($drive) {
    throw "Only Move takes a target drive."
  }
  $flags = [ordered]@{}
  foreach ($flag in @("setSparse", "skipTrim", "verifyBoot")) {
    $value = Get-Field $Object $flag $false
    if ($value -isnot [bool]) { throw "Request field '$flag' must be true or false." }
    $flags[$flag] = $value
  }
  return [ordered]@{
    id = $id; operation = $operation; distro = $distroName; targetDrive = $drive
    setSparse = $flags.setSparse; skipTrim = $flags.skipTrim; verifyBoot = $flags.verifyBoot
    requestedBy = [string](Get-Field $Object "requestedBy" "")
  }
}

function Get-DistroVhd {
  param([string]$Name, [string]$RegistryRoot = $script:LxssRoot)
  if (-not (Test-Path -LiteralPath $RegistryRoot)) { throw "No WSL registrations under $RegistryRoot." }
  foreach ($key in Get-ChildItem -LiteralPath $RegistryRoot) {
    $props = Get-ItemProperty -LiteralPath $key.PSPath
    if ((Get-Field $props "DistributionName" "") -ne $Name) { continue }
    if ([int](Get-Field $props "Version" 0) -ne 2) { throw "'$Name' is not a WSL2 distribution." }
    $base = ([string](Get-Field $props "BasePath" "")) -replace '^\\\\\?\\', ''
    if (-not $base) { throw "'$Name' has no BasePath in its WSL registration." }
    $file = [string](Get-Field $props "VhdFileName" "ext4.vhdx")
    if (-not $file) { $file = "ext4.vhdx" }
    return [PSCustomObject]@{ Name = $Name; Path = (Join-Path $base $file) }
  }
  throw "WSL distribution '$Name' is not registered for $env:USERNAME."
}

function Assert-VhdPresent {
  param($Vhd)
  if (-not (Test-Path -LiteralPath $Vhd.Path -PathType Leaf)) {
    throw "'$($Vhd.Name)' is registered, but its VHD is missing at $($Vhd.Path) (dangling registration); nothing to maintain."
  }
}

function Invoke-Wsl {
  param([string[]]$Arguments)
  # Windows PowerShell 5.1 turns redirected native stderr into ErrorRecords;
  # with Stop they would abort the script instead of reporting an exit code.
  $ErrorActionPreference = "Continue"
  $output = @(& wsl.exe @Arguments 2>&1 | ForEach-Object { "$_" })
  $code = $LASTEXITCODE
  return [PSCustomObject]@{ ExitCode = $code; Text = ((($output -join "`n") -replace "`0", "").Trim()) }
}

function Get-RunningDistroNames {
  $result = Invoke-Wsl @("--list", "--running", "--quiet")
  if ($result.ExitCode -ne 0) {
    if ($result.Text -match '(?i)no running') { return @() }
    throw "Could not list running WSL distributions (exit $($result.ExitCode)): $($result.Text)"
  }
  return @($result.Text -split "`r?`n" | ForEach-Object { $_.Trim() } | Where-Object { $_ })
}

function Wait-DistroStopped {
  param([string]$Name, [int]$TimeoutSeconds = 60, [int]$StableSeconds = 3)
  $deadline = (Get-Date).AddSeconds($TimeoutSeconds)
  $since = $null
  while ((Get-Date) -lt $deadline) {
    if (@(Get-RunningDistroNames) -notcontains $Name) {
      if ($null -eq $since) { $since = Get-Date }
      if (((Get-Date) - $since).TotalSeconds -ge $StableSeconds) { return $true }
    } else {
      $since = $null
    }
    Start-Sleep -Milliseconds 500
  }
  return $false
}

function Test-FileReleased {
  param([string]$Path)
  try {
    $stream = [IO.File]::Open($Path, [IO.FileMode]::Open, [IO.FileAccess]::Read, [IO.FileShare]::None)
    $stream.Dispose()
    return $true
  } catch {
    return $false
  }
}

function Wait-FileReleased {
  param([string]$Path, [int]$TimeoutSeconds = 60)
  $deadline = (Get-Date).AddSeconds($TimeoutSeconds)
  while ((Get-Date) -lt $deadline) {
    if (Test-FileReleased $Path) { return $true }
    Start-Sleep -Seconds 1
  }
  return $false
}

function Get-SpaceInfo {
  param([string]$Path)
  $item = Get-Item -LiteralPath $Path
  $drive = New-Object System.IO.DriveInfo ([System.IO.Path]::GetPathRoot($item.FullName))
  return [ordered]@{
    fileBytes = [int64]$item.Length
    volume = $drive.Name
    fileSystem = $drive.DriveFormat
    volumeFreeBytes = [int64]$drive.AvailableFreeSpace
    volumeTotalBytes = [int64]$drive.TotalSize
  }
}

function Format-GiB {
  param([double]$Bytes)
  return ("{0:N1} GiB" -f ($Bytes / 1GB))
}

# A VHDX's virtual size is the most it can grow to. If that exceeds the
# volume holding it, the file can fill the volume while the guest still has
# free space, and then WSL cannot boot reliably. Keep a margin.
$script:CapacityMarginBytes = [int64]50GB

# Size of the root filesystem's block device from /proc/self/mountinfo and
# /proc/partitions text (one call, no admin rights, works while running).
function Get-RootDeviceBytes {
  param([string]$Text)
  $device = $null
  foreach ($line in ($Text -split "`r?`n")) {
    $halves = $line -split ' - ', 2
    if ($halves.Count -ne 2) { continue }
    $left = $halves[0].Trim() -split '\s+'
    $right = $halves[1].Trim() -split '\s+'
    if ($left.Count -ge 5 -and $left[4] -eq '/' -and $right.Count -ge 2 -and $right[1] -match '^/dev/([A-Za-z0-9]+)$') {
      $device = $Matches[1]
    }
  }
  if (-not $device) { return $null }
  foreach ($line in ($Text -split "`r?`n")) {
    $fields = $line.Trim() -split '\s+'
    if ($fields.Count -eq 4 -and $fields[3] -eq $device -and $fields[2] -match '^\d+$') {
      return ([int64]$fields[2] * 1024)
    }
  }
  return $null
}

function Get-VirtualDiskBytes {
  param([string]$Name, [string]$VhdPath, [bool]$Running)
  if ($Running) {
    $result = Invoke-Wsl @("-d", $Name, "--exec", "/bin/sh", "-c", "cat /proc/self/mountinfo /proc/partitions")
    if ($result.ExitCode -eq 0) {
      $bytes = Get-RootDeviceBytes $result.Text
      if ($bytes) { return $bytes }
    }
  }
  if ((Test-Administrator) -and (Get-Module -ListAvailable -Name Hyper-V) -and (Test-FileReleased $VhdPath)) {
    try {
      Import-Module Hyper-V -ErrorAction Stop
      return [int64](Get-VHD -Path $VhdPath -ErrorAction Stop).Size
    } catch { }
  }
  return $null
}

function Get-CapacityCheck {
  # Two ways a VHD can fill its volume while ext4 still has free space: its
  # ceiling exceeds the volume (structural), or the growth still allowed
  # (ceiling minus current file) exceeds the volume's free space, which other
  # files share (current). Both keep a margin.
  param($VirtualBytes, $Space, [int64]$MarginBytes = $script:CapacityMarginBytes)
  if (-not $VirtualBytes) {
    return [ordered]@{ known = $false; atRisk = $null; message = "virtual size unknown (distro stopped and no elevated Get-VHD)" }
  }
  $virtual = [int64]$VirtualBytes
  $limit = [int64]$Space.volumeTotalBytes - $MarginBytes
  $growth = [int64]([Math]::Max([int64]0, $virtual - [int64]$Space.fileBytes))
  $ceilingRisk = $virtual -gt $limit
  $growthRisk = $growth -gt ([int64]$Space.volumeFreeBytes - $MarginBytes)
  $problems = New-Object System.Collections.ArrayList
  if ($ceilingRisk) {
    [void]$problems.Add(("ceiling {0} exceeds volume {1} minus {2}; keep it at most {3} (resize is a separate maintenance)" -f (Format-GiB $virtual), (Format-GiB $Space.volumeTotalBytes), (Format-GiB $MarginBytes), (Format-GiB $limit)))
  }
  if ($growthRisk) {
    [void]$problems.Add(("it may still grow {0} but the volume has {1} free (margin {2}); compact it or free space" -f (Format-GiB $growth), (Format-GiB $Space.volumeFreeBytes), (Format-GiB $MarginBytes)))
  }
  $message = $(if ($problems.Count) { "VHD can fill its volume: " + ($problems -join "; ") } else {
      "ceiling {0} and remaining growth {1} fit volume {2} ({3} free) with a {4} margin" -f (Format-GiB $virtual), (Format-GiB $growth), (Format-GiB $Space.volumeTotalBytes), (Format-GiB $Space.volumeFreeBytes), (Format-GiB $MarginBytes)
    })
  return [ordered]@{
    known = $true; atRisk = ($ceilingRisk -or $growthRisk); ceilingRisk = $ceilingRisk; growthRisk = $growthRisk
    virtualBytes = $virtual; growthBytes = $growth; volumeTotalBytes = [int64]$Space.volumeTotalBytes
    volumeFreeBytes = [int64]$Space.volumeFreeBytes; marginBytes = $MarginBytes; maxSafeBytes = $limit; message = $message
  }
}

function Add-Step {
  param([string]$Name, [bool]$Ok, [string]$Detail = "")
  if ($Ok) { Write-Line ok "$Name $Detail" } else { Write-Line warn "$Name FAILED $Detail" }
  if ($null -ne $script:Receipt) {
    [void]$script:Receipt.steps.Add([ordered]@{
      name = $Name; ok = $Ok; at = (Get-Date).ToString("o"); detail = $Detail
    })
    Save-Receipt
  }
}

function Save-Receipt {
  if ($null -eq $script:Receipt -or -not $script:ReceiptPath) { return }
  $json = $script:Receipt | ConvertTo-Json -Depth 8
  $tmp = "$($script:ReceiptPath).tmp"
  [IO.File]::WriteAllText($tmp, $json, $script:Utf8NoBom)
  Move-Item -LiteralPath $tmp -Destination $script:ReceiptPath -Force
  Copy-Item -LiteralPath $script:ReceiptPath -Destination (Join-Path $StateDir "last.json") -Force
}

function Suspend-Watchdog {
  param([string]$Id)
  if (Test-Path -LiteralPath $script:PausePath) {
    throw "A WSL watchdog pause marker already exists ($($script:PausePath)); another maintenance may be running."
  }
  New-Item -ItemType Directory -Force -Path (Split-Path -Parent $script:PausePath) | Out-Null
  Set-Content -LiteralPath $script:PausePath -Value "devbox-maintenance $Id $(Get-Date -Format o)" -Encoding ASCII
  $script:CreatedPause = $true
  Add-Step "watchdog-paused" $true $script:PausePath
}

function Update-WatchdogPause {
  # The watchdog expires markers older than its PauseMaxHours (default 2h);
  # long compactions keep the marker fresh.
  if ($script:CreatedPause -and (Test-Path -LiteralPath $script:PausePath)) {
    (Get-Item -LiteralPath $script:PausePath).LastWriteTime = Get-Date
  }
}

function Resume-Watchdog {
  if ($script:CreatedPause) {
    Remove-Item -LiteralPath $script:PausePath -Force -ErrorAction SilentlyContinue
    $script:CreatedPause = $false
    Add-Step "watchdog-resumed" $true ""
  }
}

function Stop-TargetDistro {
  param([string]$Name, [bool]$ShutdownWsl, [string]$VhdPath)
  $result = Invoke-Wsl @("--terminate", $Name)
  Add-Step "terminate" $true "exit $($result.ExitCode) $($result.Text)"
  if ($ShutdownWsl) {
    $result = Invoke-Wsl @("--shutdown")
    if ($result.ExitCode -ne 0) { throw "wsl --shutdown failed (exit $($result.ExitCode)): $($result.Text)" }
    Add-Step "wsl-shutdown" $true ""
  }
  if (-not (Wait-DistroStopped $Name)) {
    throw "'$Name' did not stay stopped; a background task may be restarting it."
  }
  if (-not (Wait-FileReleased $VhdPath)) { throw "The VHD is still in use after '$Name' stopped: $VhdPath" }
  Add-Step "stopped" $true $VhdPath
}

function Start-TargetDistro {
  param([string]$Name)
  $result = Invoke-Wsl @("-d", $Name, "--exec", "/bin/sh", "-c", "true")
  if ($result.ExitCode -ne 0) { throw "Could not start '$Name' (exit $($result.ExitCode)): $($result.Text)" }
  Add-Step "started" $true $Name
}

function Invoke-GuestTrim {
  param([string]$Name)
  $last = ""
  foreach ($path in @("/run/current-system/sw/bin/fstrim", "/usr/sbin/fstrim", "/sbin/fstrim")) {
    $result = Invoke-Wsl @("-d", $Name, "-u", "root", "--exec", $path, "-v", "/")
    if ($result.ExitCode -eq 0) { Add-Step "trim" $true $result.Text; return }
    $last = "exit $($result.ExitCode) $($result.Text)"
  }
  Add-Step "trim" $false "continuing without a fresh trim: $last"
}

$script:OptimizeWork = {
  param($Path, $Mode)
  $ErrorActionPreference = "Stop"
  Import-Module Hyper-V
  Optimize-VHD -Path $Path -Mode $Mode
  "Optimize-VHD -Mode $Mode completed"
}

$script:DiskPartWork = {
  param($Path)
  $ErrorActionPreference = "Stop"
  $commands = Join-Path $env:TEMP ("devbox-compact-" + [guid]::NewGuid().ToString("N") + ".txt")
  Set-Content -LiteralPath $commands -Value @("select vdisk file=`"$Path`"", "compact vdisk") -Encoding ASCII
  try {
    $output = & "$env:SystemRoot\System32\diskpart.exe" /s $commands
    $code = $LASTEXITCODE
  } finally {
    Remove-Item -LiteralPath $commands -Force -ErrorAction SilentlyContinue
  }
  $text = ($output -join "`n")
  if ($code -ne 0 -or $text -notmatch '(?i)successfully compacted') { throw "diskpart exit ${code}:`n$text" }
  $text
}

function Invoke-WorkJob {
  param([string]$Label, [scriptblock]$Work, [object[]]$Arguments, [string]$VhdPath)
  $job = Start-Job -ScriptBlock $Work -ArgumentList $Arguments
  $start = Get-Date
  try {
    while ($job.State -eq "Running" -or $job.State -eq "NotStarted") {
      Update-WatchdogPause
      $elapsed = (Get-Date) - $start
      $image = $(if (Test-Path -LiteralPath $VhdPath) { Format-GiB (Get-Item -LiteralPath $VhdPath).Length } else { "-" })
      Write-Line info ("{0}: {1:hh\:mm\:ss} elapsed, image {2}" -f $Label, $elapsed, $image)
      [void](Wait-Job -Job $job -Timeout 60)
    }
    $jobErrors = $null
    $output = @(Receive-Job -Job $job -ErrorAction SilentlyContinue -ErrorVariable jobErrors)
    $detail = (($output | ForEach-Object { "$_" }) -join "`n").Trim()
    $errorText = ((@($jobErrors) | Where-Object { $_ } | ForEach-Object { "$_" }) -join "`n").Trim()
    if ($errorText) { $detail = ("$detail`n$errorText").Trim() }
    $ok = ($job.State -eq "Completed") -and (-not $errorText)
    return [PSCustomObject]@{ Ok = $ok; Detail = $detail; Seconds = [int]((Get-Date) - $start).TotalSeconds }
  } finally {
    Remove-Job -Job $job -Force -ErrorAction SilentlyContinue
  }
}

function Invoke-VhdCompaction {
  param([string]$VhdPath)
  $attempts = New-Object System.Collections.ArrayList
  if (Get-Module -ListAvailable -Name Hyper-V) {
    [void]$attempts.Add(@{ Label = "Optimize-VHD Full"; Work = $script:OptimizeWork; Arguments = @($VhdPath, "Full") })
    [void]$attempts.Add(@{ Label = "Optimize-VHD Pretrimmed"; Work = $script:OptimizeWork; Arguments = @($VhdPath, "Pretrimmed") })
  }
  [void]$attempts.Add(@{ Label = "DiskPart compact"; Work = $script:DiskPartWork; Arguments = @($VhdPath) })
  $anyOk = $false
  foreach ($attempt in $attempts) {
    $before = (Get-Item -LiteralPath $VhdPath).Length
    $result = Invoke-WorkJob $attempt.Label $attempt.Work $attempt.Arguments $VhdPath
    $after = (Get-Item -LiteralPath $VhdPath).Length
    Add-Step $attempt.Label $result.Ok ("{0}s, {1} -> {2}. {3}" -f $result.Seconds, (Format-GiB $before), (Format-GiB $after), $result.Detail)
    if ($result.Ok) {
      $anyOk = $true
      # A method can succeed without seeing the guest's unused blocks; try the
      # next one only when this one returned (almost) nothing.
      if (($before - $after) -ge 1GB) { return $true }
    }
  }
  return $anyOk
}

# Every Move lands in one fixed layout, <Drive>:\WSL\<Distro> (as on the Q:
# devboxes), so a request chooses a drive, never a path.
function Get-MoveDestination {
  param([string]$Drive, [string]$Name)
  return ("{0}:\WSL\{1}" -f $Drive.ToUpperInvariant(), $Name)
}

function Test-MoveFits {
  param([int64]$FileBytes, [int64]$FreeBytes, [int64]$MarginBytes = $script:CapacityMarginBytes)
  return ($FreeBytes -ge ($FileBytes + $MarginBytes))
}

# Checks made before WSL is touched: a ready fixed drive, no file in the way,
# room for the VHD plus the margin. A VHD already on that drive needs nothing.
function Get-MovePlan {
  param($Vhd, [string]$Destination, [int64]$MarginBytes = $script:CapacityMarginBytes)
  $root = [IO.Path]::GetPathRoot($Destination)
  $drive = New-Object System.IO.DriveInfo $root
  if (-not $drive.IsReady) { throw "Drive $root is not ready." }
  if ($drive.DriveType -ne [IO.DriveType]::Fixed) { throw "Drive $root is $($drive.DriveType), not a fixed local drive." }
  $fileBytes = [int64](Get-Item -LiteralPath $Vhd.Path).Length
  $target = Join-Path $Destination ([IO.Path]::GetFileName($Vhd.Path))
  $already = ([IO.Path]::GetPathRoot($Vhd.Path)) -ieq $root
  $free = [int64]$drive.AvailableFreeSpace
  if (-not $already) {
    if (Test-Path -LiteralPath $target) { throw "$target already exists; move or remove it first." }
    if (-not (Test-MoveFits $fileBytes $free $MarginBytes)) {
      throw ("{0} has {1} free; the {2} VHD needs {3} with a {4} margin." -f $root, (Format-GiB $free), (Format-GiB $fileBytes), (Format-GiB ($fileBytes + $MarginBytes)), (Format-GiB $MarginBytes))
    }
  }
  return [PSCustomObject]@{
    Destination = $Destination; Target = $target; AlreadyThere = $already; FileBytes = $fileBytes
    # The destination volume as it will be once the VHD is on it.
    Space = [ordered]@{
      fileBytes = $fileBytes; volume = $drive.Name; fileSystem = $drive.DriveFormat
      volumeFreeBytes = $(if ($already) { $free } else { $free - $fileBytes }); volumeTotalBytes = [int64]$drive.TotalSize
    }
  }
}

$script:MoveWork = {
  param($Name, $Destination)
  $ErrorActionPreference = "Continue"
  $output = @(& wsl.exe --manage $Name --move $Destination 2>&1 | ForEach-Object { "$_" })
  $code = $LASTEXITCODE
  $text = (($output -join "`n") -replace "`0", "").Trim()
  if ($code -ne 0) { throw "wsl --manage --move exit ${code}: $text" }
  $text
}

function Invoke-DistroMove {
  param([string]$Name, $Move, [string]$SourcePath)
  # Copying a large VHD across volumes can outlast the watchdog's pause
  # expiry; Invoke-WorkJob keeps the marker fresh.
  $result = Invoke-WorkJob "move" $script:MoveWork @($Name, $Move.Destination) $Move.Target
  $moved = Get-DistroVhd $Name
  $ok = $result.Ok -and ($moved.Path -ieq $Move.Target) -and (Test-Path -LiteralPath $moved.Path -PathType Leaf)
  Add-Step "move" $ok ("{0}s, {1} -> {2}. {3}" -f $result.Seconds, $SourcePath, $moved.Path, $result.Detail)
  if (-not $ok) { throw "Move did not complete; WSL registers '$Name' at $($moved.Path)." }
}

function New-Receipt {
  param($Request, [string]$LogPath)
  $script:Receipt = [ordered]@{
    schema = 1
    id = $Request.id
    operation = $Request.operation
    distro = $Request.distro
    options = [ordered]@{ setSparse = $Request.setSparse; skipTrim = $Request.skipTrim; verifyBoot = $Request.verifyBoot; targetDrive = $Request.targetDrive }
    requestedBy = $Request.requestedBy
    state = "running"
    startedAt = (Get-Date).ToString("o")
    finishedAt = $null
    log = $LogPath
    vhd = $null
    vhdAfter = $null
    wasRunning = $null
    runningBefore = $null
    shutdownWsl = $null
    capacity = $null
    before = $null
    after = $null
    reclaimedBytes = $null
    steps = New-Object System.Collections.ArrayList
    error = $null
  }
  $script:ReceiptPath = Join-Path (Join-Path $StateDir "receipts") "$($Request.id).json"
  Save-Receipt
}

function Get-MaintenancePlan {
  # VhdReleased: nothing holds the VHD now (a stopped distro WSL has not
  # attached since its VM started). Only Move uses it.
  param($Request, [string[]]$RunningBefore, [bool]$VhdReleased = $false)
  $guardsWatchdog = ($Request.distro -eq $script:WatchdogDistro)
  # While the WSL VM runs, WSL keeps every distro's VHD attached (held by
  # System and the WSL service) even after `wsl --terminate`; compacting or
  # converting a VHD therefore needs the whole VM shut down. That stops the
  # watchdog's distro too, so pause the watchdog and restore what was running.
  $wasRunning = @($RunningBefore) -contains $Request.distro
  # A Move needs the file released too, but a stopped distro whose VHD WSL does
  # not hold can move without taking the devbox down.
  $moveNeedsShutdown = ($Request.operation -eq "Move") -and ($wasRunning -or -not $VhdReleased)
  $shutdown = $guardsWatchdog -or $Request.operation -eq "Compact" -or $Request.setSparse -or $moveNeedsShutdown
  $restart = New-Object System.Collections.ArrayList
  if ($shutdown) {
    # Docker Desktop starts its own distros; do not start them for it.
    foreach ($name in @($RunningBefore)) {
      if ($name -ne $Request.distro -and $name -notmatch '^docker-desktop') { [void]$restart.Add($name) }
    }
    if (-not $restart.Contains($script:WatchdogDistro) -and $Request.distro -ne $script:WatchdogDistro -and @($RunningBefore) -contains $script:WatchdogDistro) {
      [void]$restart.Add($script:WatchdogDistro)
    }
  }
  $startTarget = $wasRunning -or $guardsWatchdog -or $Request.verifyBoot
  return [PSCustomObject]@{
    Shutdown = $shutdown
    PauseWatchdog = $shutdown
    WasRunning = $wasRunning
    StartTarget = $startTarget
    StopTargetAfterBootCheck = ($Request.verifyBoot -and -not $wasRunning -and -not $guardsWatchdog)
    RestartOthers = @($restart)
  }
}

function Invoke-Maintenance {
  param($Request)
  $vhd = Get-DistroVhd $Request.distro
  Assert-VhdPresent $vhd
  $runningBefore = @(Get-RunningDistroNames)
  $plan = Get-MaintenancePlan $Request $runningBefore (Test-FileReleased $vhd.Path)
  $script:Receipt.vhd = $vhd.Path
  $script:Receipt.before = Get-SpaceInfo $vhd.Path
  $script:Receipt.wasRunning = $plan.WasRunning
  $script:Receipt.runningBefore = $runningBefore
  $script:Receipt.shutdownWsl = $plan.Shutdown
  $move = $null
  if ($Request.operation -eq "Move") { $move = Get-MovePlan $vhd (Get-MoveDestination $Request.targetDrive $Request.distro) }
  # For a Move, what matters is the volume the VHD lands on.
  $capacity = Get-CapacityCheck (Get-VirtualDiskBytes $Request.distro $vhd.Path $plan.WasRunning) $(if ($move) { $move.Space } else { $script:Receipt.before })
  $script:Receipt.capacity = $capacity
  if ($capacity.known) { Add-Step "capacity" (-not $capacity.atRisk) $capacity.message }
  if ($move -and $move.AlreadyThere) {
    Add-Step "move" $true "'$($Request.distro)' is already on $($Request.targetDrive): ($($vhd.Path)); nothing to do"
    $script:Receipt.vhdAfter = $vhd.Path
    $script:Receipt.after = $script:Receipt.before
    $script:Receipt.reclaimedBytes = [int64]0
    Save-Receipt
    return
  }
  Save-Receipt
  $failure = $null
  try {
    if ($Request.operation -eq "Compact" -and -not $Request.skipTrim) { Invoke-GuestTrim $Request.distro }
    if ($plan.PauseWatchdog) { Suspend-Watchdog $Request.id }
    Stop-TargetDistro -Name $Request.distro -ShutdownWsl $plan.Shutdown -VhdPath $vhd.Path
    if ($Request.operation -eq "Compact") {
      if (-not (Invoke-VhdCompaction $vhd.Path)) { throw "No compaction method succeeded." }
    }
    if ($Request.operation -eq "Move") { Invoke-DistroMove $Request.distro $move $vhd.Path }
    if ($Request.setSparse) {
      $result = Invoke-Wsl @("--manage", $Request.distro, "--set-sparse", "true")
      Add-Step "set-sparse" ($result.ExitCode -eq 0) "exit $($result.ExitCode) $($result.Text)"
      if ($result.ExitCode -ne 0) { throw "wsl --manage --set-sparse failed: $($result.Text)" }
    }
  } catch {
    $failure = "$_"
    Add-Step "maintenance" $false $failure
  } finally {
    # Always bring the devbox back, whatever happened above: first the
    # distros the shutdown stopped (NixOS first), then the target.
    foreach ($name in @($plan.RestartOthers | Sort-Object { $_ -ne $script:WatchdogDistro })) {
      try { Start-TargetDistro $name } catch {
        if (-not $failure) { $failure = "$_" }
        Add-Step "start" $false "$_"
      }
    }
    if ($plan.StartTarget) {
      try {
        Start-TargetDistro $Request.distro
        if ($plan.StopTargetAfterBootCheck) {
          $result = Invoke-Wsl @("--terminate", $Request.distro)
          Add-Step "restored-stopped" ($result.ExitCode -eq 0) "boot verified; left stopped as found"
        }
      } catch {
        if (-not $failure) { $failure = "$_" }
        Add-Step "start" $false "$_"
      }
    }
    Resume-Watchdog
    # A Move changes where the VHD lives; report it from the registration.
    $afterPath = $vhd.Path
    try { $afterPath = (Get-DistroVhd $Request.distro).Path } catch { }
    $script:Receipt.vhdAfter = $afterPath
    if (Test-Path -LiteralPath $afterPath -PathType Leaf) {
      $script:Receipt.after = Get-SpaceInfo $afterPath
      $script:Receipt.reclaimedBytes = [int64]($script:Receipt.before.fileBytes - $script:Receipt.after.fileBytes)
    }
    Save-Receipt
  }
  if ($failure) { throw $failure }
}

function Invoke-Request {
  param($Request)
  if ($Request.operation -eq "Compact" -and -not (Test-Administrator)) {
    throw "Compact needs an elevated PowerShell (or the installed DevboxMaintenance task)."
  }
  foreach ($dir in @($StateDir, (Join-Path $StateDir "receipts"), (Join-Path $StateDir "logs"))) {
    New-Item -ItemType Directory -Force -Path $dir | Out-Null
  }
  $lockPath = Join-Path $StateDir "run.lock"
  try {
    $lock = [IO.File]::Open($lockPath, [IO.FileMode]::OpenOrCreate, [IO.FileAccess]::ReadWrite, [IO.FileShare]::None)
  } catch {
    throw "Another devbox maintenance run holds $lockPath."
  }
  $log = Join-Path (Join-Path $StateDir "logs") "$($Request.id).log"
  $transcript = $false
  try {
    try { Start-Transcript -LiteralPath $log -Append | Out-Null; $transcript = $true } catch { }
    New-Receipt $Request $log
    Write-Line info "$($Request.operation) '$($Request.distro)' (request $($Request.id))"
    try {
      Invoke-Maintenance $Request
      $script:Receipt.state = "succeeded"
    } catch {
      $script:Receipt.state = "failed"
      $script:Receipt.error = "$_"
    }
    $script:Receipt.finishedAt = (Get-Date).ToString("o")
    Save-Receipt
    if ($script:Receipt.state -eq "succeeded") {
      Write-Line ok ("Done. {0}: image {1} -> {2}; {3} free on {4}." -f $script:Receipt.vhdAfter, (Format-GiB $script:Receipt.before.fileBytes), (Format-GiB $script:Receipt.after.fileBytes), (Format-GiB $script:Receipt.after.volumeFreeBytes), $script:Receipt.after.volume)
    } else {
      Write-Line error "Failed: $($script:Receipt.error). Receipt: $($script:ReceiptPath)"
    }
    return ($script:Receipt.state -eq "succeeded")
  } finally {
    if ($transcript) { try { Stop-Transcript | Out-Null } catch { } }
    $lock.Dispose()
  }
}

function Get-DryRunReport {
  param($Request)
  $vhd = Get-DistroVhd $Request.distro
  Assert-VhdPresent $vhd
  $task = Get-ScheduledTask -TaskName $script:TaskName -ErrorAction SilentlyContinue
  $installedHash = $null
  if (Test-Path -LiteralPath $script:InstalledScript) {
    $installedHash = (Get-FileHash -LiteralPath $script:InstalledScript -Algorithm SHA256).Hash
  }
  $space = Get-SpaceInfo $vhd.Path
  $running = (@(Get-RunningDistroNames) -contains $Request.distro)
  $move = $null
  $moveError = $null
  if ($Request.operation -eq "Move") {
    try { $move = Get-MovePlan $vhd (Get-MoveDestination $Request.targetDrive $Request.distro) } catch { $moveError = "$_" }
  }
  return [ordered]@{
    operation = $Request.operation
    distro = $Request.distro
    vhd = $vhd.Path
    space = $space
    move = $move
    moveError = $moveError
    capacity = Get-CapacityCheck (Get-VirtualDiskBytes $Request.distro $vhd.Path $running) $(if ($move) { $move.Space } else { $space })
    running = $running
    guardsWatchdog = ($Request.distro -eq $script:WatchdogDistro)
    plan = Get-MaintenancePlan $Request @(Get-RunningDistroNames) (Test-FileReleased $vhd.Path)
    watchdogPaused = (Test-Path -LiteralPath $script:PausePath)
    elevated = (Test-Administrator)
    hyperV = [bool](Get-Module -ListAvailable -Name Hyper-V)
    taskInstalled = [bool]$task
    taskState = $(if ($task) { [string]$task.State } else { $null })
    installedScriptSha256 = $installedHash
    thisScriptSha256 = $(if ($PSCommandPath) { (Get-FileHash -LiteralPath $PSCommandPath -Algorithm SHA256).Hash } else { $null })
  }
}

function Install-MaintenanceTask {
  if (-not (Test-Administrator)) { throw "-Install needs an elevated PowerShell (one UAC consent)." }
  New-Item -ItemType Directory -Force -Path $script:InstallDir | Out-Null
  # Administrators and SYSTEM may change the code the elevated task runs;
  # users may only read it. Requests stay in the user's profile.
  & icacls.exe $script:InstallDir /inheritance:r /grant:r "*S-1-5-18:(OI)(CI)F" "*S-1-5-32-544:(OI)(CI)F" "*S-1-5-32-545:(OI)(CI)RX" | Out-Null
  if ($LASTEXITCODE -ne 0) { throw "Could not restrict $($script:InstallDir)." }
  Copy-Item -LiteralPath $PSCommandPath -Destination $script:InstalledScript -Force
  $powershell = Join-Path $env:SystemRoot "System32\WindowsPowerShell\v1.0\powershell.exe"
  $action = New-ScheduledTaskAction -Execute $powershell -Argument ("-NoProfile -NonInteractive -ExecutionPolicy Bypass -File `"{0}`" -FromTask" -f $script:InstalledScript)
  $user = $(if ($TaskUser) { $TaskUser } else { "{0}\{1}" -f $env:USERDOMAIN, $env:USERNAME })
  $principal = New-ScheduledTaskPrincipal -UserId $user -LogonType Interactive -RunLevel Highest
  $settings = New-ScheduledTaskSettingsSet -AllowStartIfOnBatteries -DontStopIfGoingOnBatteries -MultipleInstances IgnoreNew -ExecutionTimeLimit (New-TimeSpan -Hours 12)
  Register-ScheduledTask -TaskName $script:TaskName -Action $action -Principal $principal -Settings $settings -Force `
    -Description "Collective devbox maintenance: runs one queued request from %LOCALAPPDATA%\devbox\maintenance\request.json." | Out-Null
  $hash = (Get-FileHash -LiteralPath $script:InstalledScript -Algorithm SHA256).Hash
  Write-Line ok "Installed task '$($script:TaskName)' for $user running $($script:InstalledScript) (sha256 $hash)."
}

function Invoke-FromTask {
  $requestPath = Join-Path $StateDir "request.json"
  if (-not (Test-Path -LiteralPath $requestPath)) { Write-Line warn "No queued request at $requestPath."; return $true }
  $queued = Join-Path (Join-Path $StateDir "requests") ("taken-{0}.json" -f [guid]::NewGuid().ToString("N"))
  New-Item -ItemType Directory -Force -Path (Split-Path -Parent $queued) | Out-Null
  # Take the request before acting so a re-trigger cannot repeat it.
  Move-Item -LiteralPath $requestPath -Destination $queued -Force
  $request = ConvertTo-MaintenanceRequest (Get-Content -Raw -LiteralPath $queued | ConvertFrom-Json)
  return (Invoke-Request $request)
}

function Invoke-Main {
  switch ($script:Mode) {
    "Install" { Install-MaintenanceTask; exit 0 }
    "Task" { if (Invoke-FromTask) { exit 0 } else { exit 1 } }
    default {
      $request = ConvertTo-MaintenanceRequest ([PSCustomObject]@{
        id = ("direct-" + [guid]::NewGuid().ToString("N").Substring(0, 12))
        operation = $Operation; distro = $Distro; targetDrive = $TargetDrive
        setSparse = [bool]$SetSparse; skipTrim = [bool]$SkipTrim; verifyBoot = [bool]$VerifyBoot
        requestedBy = "console"
      })
      if ($DryRun) { Get-DryRunReport $request | ConvertTo-Json -Depth 6; exit 0 }
      if ($Operation -eq "Move") {
        # Refuse an unusable target before asking; nothing to confirm if already there.
        $move = Get-MovePlan (Get-DistroVhd $Distro) (Get-MoveDestination $request.targetDrive $Distro)
        if ($move.AlreadyThere) { Write-Line ok "'$Distro' is already on $($request.targetDrive): ($((Get-DistroVhd $Distro).Path)); nothing to do."; exit 0 }
      }
      if (-not $Force) {
        $plan = Get-MaintenancePlan $request @(Get-RunningDistroNames) (Test-FileReleased (Get-DistroVhd $Distro).Path)
        $scope = $(if ($plan.Shutdown) { 'shuts down WSL (every distribution, including the devbox)' } else { "stops '$Distro'" })
        if ($Operation -eq "Move") { $scope = "moves '$Distro' to $(Get-MoveDestination $request.targetDrive $Distro) and $scope" }
        Write-Line warn "$Operation $scope; work inside is interrupted, and running distributions are restarted afterwards."
        $answer = Read-Host "Continue? (y/N)"
        if ($answer -notmatch '^[Yy]') { Write-Line warn "Aborted; nothing changed."; exit 1 }
      }
      if (Invoke-Request $request) { exit 0 } else { exit 1 }
    }
  }
}

if (-not $NoMain) { Invoke-Main }
