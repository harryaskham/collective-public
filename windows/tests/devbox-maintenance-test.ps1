# devbox-maintenance.ps1 tests: Windows PowerShell 5.1 and PowerShell 7, no admin,
# no WSL or scheduled-task changes. Files go to a unique temp directory and the
# fake WSL registrations to a unique HKCU test key; both are removed on exit.
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
. (Join-Path $PSScriptRoot '..\devbox-maintenance.ps1') -NoMain

function Assert-True($Description, $Condition) {
  if (-not $Condition) { throw "FAIL: $Description" }
  Write-Host "ok - $Description"
}

function Assert-Throws($Description, [scriptblock]$Block, [string]$Pattern) {
  try { & $Block } catch {
    if ("$_" -notmatch $Pattern) { throw "FAIL: $Description (wrong error: $_)" }
    Write-Host "ok - $Description"
    return
  }
  throw "FAIL: $Description (no error)"
}

$testRoot = Join-Path ([IO.Path]::GetTempPath()) ('devbox maintenance (test) ' + [guid]::NewGuid().ToString('N'))
$registryRoot = 'HKCU:\Software\CollectiveDevboxMaintenanceTest-' + [guid]::NewGuid().ToString('N')
try {
  New-Item -ItemType Directory -Path $testRoot | Out-Null

  # Requests: only whitelisted operations, plain names and real booleans.
  $valid = ConvertTo-MaintenanceRequest ([PSCustomObject]@{
    id = '20261021T120000Z-abc123'; operation = 'Compact'; distro = 'Ubuntu2204'
    setSparse = $false; verifyBoot = $true; requestedBy = 'test'
  })
  Assert-True 'valid request normalizes defaults' ($valid.distro -eq 'Ubuntu2204' -and $valid.verifyBoot -and -not $valid.skipTrim)
  $parsed = ConvertTo-MaintenanceRequest ('{"id":"abcdefgh","operation":"Cycle","distro":"NixOS","skipTrim":true}' | ConvertFrom-Json)
  Assert-True 'JSON request parses booleans' ($parsed.operation -eq 'Cycle' -and $parsed.skipTrim -eq $true)
  Assert-Throws 'unknown operation is rejected' { ConvertTo-MaintenanceRequest ([PSCustomObject]@{ id = 'abcdefgh'; operation = 'Unregister'; distro = 'NixOS' }) } 'not one of'
  foreach ($bad in @('NixOS; wsl --unregister', '..\x', 'a b', '')) {
    Assert-Throws "distro '$bad' is rejected" { ConvertTo-MaintenanceRequest ([PSCustomObject]@{ id = 'abcdefgh'; operation = 'Cycle'; distro = $bad }) } 'Distro name'
  }
  Assert-Throws 'short id is rejected' { ConvertTo-MaintenanceRequest ([PSCustomObject]@{ id = 'x'; operation = 'Cycle'; distro = 'NixOS' }) } 'id'
  Assert-Throws 'string flag is rejected' { ConvertTo-MaintenanceRequest ([PSCustomObject]@{ id = 'abcdefgh'; operation = 'Cycle'; distro = 'NixOS'; setSparse = 'true' }) } 'true or false'

  # VHDX path comes from the WSL registration, including \\?\ BasePaths.
  $lxss = Join-Path $registryRoot 'Lxss'
  New-Item -Path $lxss -Force | Out-Null
  $one = New-Item -Path (Join-Path $lxss ('{' + [guid]::NewGuid().ToString() + '}'))
  New-ItemProperty -Path $one.PSPath -Name DistributionName -Value 'Canary' | Out-Null
  New-ItemProperty -Path $one.PSPath -Name BasePath -Value '\\?\C:\Users\x\WSL\Canary' | Out-Null
  New-ItemProperty -Path $one.PSPath -Name Version -Value 2 -PropertyType DWord | Out-Null
  $two = New-Item -Path (Join-Path $lxss ('{' + [guid]::NewGuid().ToString() + '}'))
  New-ItemProperty -Path $two.PSPath -Name DistributionName -Value 'Legacy' | Out-Null
  New-ItemProperty -Path $two.PSPath -Name BasePath -Value 'C:\legacy' | Out-Null
  New-ItemProperty -Path $two.PSPath -Name Version -Value 1 -PropertyType DWord | Out-Null
  $vhd = Get-DistroVhd -Name 'Canary' -RegistryRoot $lxss
  Assert-True 'BasePath prefix stripped and default VHD name used' ($vhd.Path -eq 'C:\Users\x\WSL\Canary\ext4.vhdx')
  Assert-Throws 'WSL1 distro is rejected' { Get-DistroVhd -Name 'Legacy' -RegistryRoot $lxss } 'not a WSL2'
  Assert-Throws 'unknown distro is rejected' { Get-DistroVhd -Name 'Missing' -RegistryRoot $lxss } 'not registered'
  Assert-Throws 'dangling registration (missing VHD) is reported' { Assert-VhdPresent $vhd } 'dangling registration'
  $present = Join-Path $testRoot 'ext4.vhdx'
  [IO.File]::WriteAllBytes($present, [byte[]](0))
  Assert-VhdPresent ([PSCustomObject]@{ Name = 'Canary'; Path = $present })
  Write-Host 'ok - present VHD passes'

  # Receipts are written atomically with every step, plus last.json.
  $StateDir = Join-Path $testRoot 'state'
  New-Item -ItemType Directory -Path (Join-Path $StateDir 'receipts') -Force | Out-Null
  New-Receipt $valid (Join-Path $testRoot 'log.txt')
  Add-Step 'first' $true 'detail'
  Add-Step 'second' $false 'oops'
  $receipt = Get-Content -Raw -LiteralPath (Join-Path $StateDir "receipts\$($valid.id).json") | ConvertFrom-Json
  Assert-True 'receipt records steps in order' ($receipt.state -eq 'running' -and @($receipt.steps).Count -eq 2 -and $receipt.steps[1].ok -eq $false)
  Assert-True 'last.json mirrors the receipt' ((Get-Content -Raw -LiteralPath (Join-Path $StateDir 'last.json') | ConvertFrom-Json).id -eq $valid.id)
  $script:Receipt = $null

  # Pause marker: created once, refused when another exists, removed on resume.
  $script:PausePath = Join-Path $testRoot 'devbox\wsl-watchdog.pause'
  Suspend-Watchdog 'abcdefgh'
  Assert-True 'pause marker created' (Test-Path -LiteralPath $script:PausePath)
  $script:CreatedPause = $false
  Assert-Throws 'existing foreign pause is respected' { Suspend-Watchdog 'abcdefgh' } 'already exists'
  $script:CreatedPause = $true
  Resume-Watchdog
  Assert-True 'pause marker removed on resume' (-not (Test-Path -LiteralPath $script:PausePath))

  # Running-distro parsing with a stubbed wsl.exe wrapper.
  function Invoke-Wsl { param([string[]]$Arguments) [PSCustomObject]@{ ExitCode = 1; Text = 'There are no running distributions.' } }
  Assert-True 'no running distributions parses as empty' (@(Get-RunningDistroNames).Count -eq 0)
  function Invoke-Wsl { param([string[]]$Arguments) [PSCustomObject]@{ ExitCode = 0; Text = "NixOS`r`nUbuntu2204`r`n" } }
  Assert-True 'running distributions are listed' ((@(Get-RunningDistroNames) -join ',') -eq 'NixOS,Ubuntu2204')
  function Invoke-Wsl { param([string[]]$Arguments) [PSCustomObject]@{ ExitCode = 5; Text = 'Catastrophic failure' } }
  Assert-Throws 'unknown wsl failure is not treated as stopped' { Get-RunningDistroNames } 'Could not list'

  # Capacity guard: virtual size from mountinfo/partitions, compared with the
  # holding volume minus a margin (ms-dev-7: 1536 GB VHD on a 1535 GB Q:).
  $procText = @(
    '24 1 8:48 / / rw,relatime shared:1 - ext4 /dev/sdd rw,discard',
    '30 24 0:5 / /dev rw - devtmpfs none rw',
    'major minor  #blocks  name',
    '   8       48 1610612736 sdd',
    '   8       32    1048576 sdc'
  ) -join "`n"
  Assert-True 'root device size parsed from mountinfo and partitions' ((Get-RootDeviceBytes $procText) -eq ([int64]1610612736 * 1024))
  Assert-True 'no root device yields null' ($null -eq (Get-RootDeviceBytes "major minor  #blocks  name`n   8 48 10 sdd"))
  $q = [int64]1648126590976
  function Space($File, $Free) { [ordered]@{ fileBytes = [int64]$File; volumeFreeBytes = [int64]$Free; volumeTotalBytes = $q } }
  # ms-dev-7 before compaction: 1536 GiB ceiling on a 1535 GiB Q:.
  $check = Get-CapacityCheck ([int64]1536GB) (Space 1476GB 2GB)
  Assert-True '1536 GiB VHD on a 1535 GiB volume is at risk (ceiling)' ($check.known -and $check.atRisk -and $check.ceilingRisk -and $check.message -match 'can fill its volume')
  # ms-dev-8 today: 1238 GiB file may grow 298 GiB; Q: has 272 GiB free.
  $check = Get-CapacityCheck ([int64]1536GB) (Space 1238GB 272GB)
  Assert-True 'remaining growth beyond free space is at risk (growth)' ($check.growthRisk -and $check.growthBytes -eq [int64]298GB)
  # A C: devbox near its ceiling with ample free space is safe.
  $c = [int64]2198429741056
  $check = Get-CapacityCheck ([int64]1536GB) ([ordered]@{ fileBytes = [int64]1512GB; volumeFreeBytes = [int64]133GB; volumeTotalBytes = $c })
  Assert-True 'small remaining growth within free space is safe' ($check.known -and -not $check.atRisk -and -not $check.growthRisk)
  $check = Get-CapacityCheck ([int64]1400GB) (Space 600GB 900GB)
  Assert-True 'ceiling 50 GiB below the volume with room is safe' (-not $check.atRisk -and $check.maxSafeBytes -eq ($q - 50GB))
  Assert-True 'unknown virtual size is reported, not guessed' (-not (Get-CapacityCheck $null (Space 1 1)).known)

  # Plan: compaction always shuts WSL down (pausing the watchdog) and restores
  # what was running; Cycle of another distro touches only that distro.
  function New-TestRequest($Operation, $Name, $Boot = $false, $Sparse = $false) {
    ConvertTo-MaintenanceRequest ([PSCustomObject]@{ id = 'abcdefgh'; operation = $Operation; distro = $Name; verifyBoot = $Boot; setSparse = $Sparse })
  }
  $plan = Get-MaintenancePlan (New-TestRequest 'Compact' 'CollectiveCanary' $true) @('NixOS', 'docker-desktop')
  Assert-True 'canary compaction shuts WSL down and pauses the watchdog' ($plan.Shutdown -and $plan.PauseWatchdog)
  Assert-True 'canary compaction restores NixOS but not Docker''s distro' ((@($plan.RestartOthers) -join ',') -eq 'NixOS')
  Assert-True 'stopped canary is boot-checked then stopped again' ($plan.StartTarget -and $plan.StopTargetAfterBootCheck)
  $plan = Get-MaintenancePlan (New-TestRequest 'Cycle' 'CollectiveCanary') @('NixOS')
  Assert-True 'cycling another distro leaves WSL and the watchdog alone' (-not $plan.Shutdown -and -not $plan.PauseWatchdog -and @($plan.RestartOthers).Count -eq 0 -and -not $plan.StartTarget)
  $plan = Get-MaintenancePlan (New-TestRequest 'Cycle' 'CollectiveCanary' $false $true) @('NixOS')
  Assert-True 'set-sparse needs a shutdown even for Cycle' ($plan.Shutdown -and (@($plan.RestartOthers) -join ',') -eq 'NixOS')
  $plan = Get-MaintenancePlan (New-TestRequest 'Compact' 'NixOS') @('NixOS', 'Other')
  Assert-True 'devbox compaction restarts NixOS and other running distros' ($plan.Shutdown -and $plan.StartTarget -and -not $plan.StopTargetAfterBootCheck -and (@($plan.RestartOthers) -join ',') -eq 'Other')
  $plan = Get-MaintenancePlan (New-TestRequest 'Compact' 'NixOS') @()
  Assert-True 'devbox distro is started even if it was down' ($plan.StartTarget)

  # A file held open exclusively is not released; a closed one is.
  $file = Join-Path $testRoot 'image.vhdx'
  [IO.File]::WriteAllBytes($file, [byte[]](1, 2, 3))
  $held = [IO.File]::Open($file, 'Open', 'Read', 'None')
  try { Assert-True 'held file is not released' (-not (Test-FileReleased $file)) } finally { $held.Dispose() }
  Assert-True 'closed file is released' (Test-FileReleased $file)

  Write-Host 'PASS: devbox-maintenance'
} finally {
  Remove-Item -LiteralPath $testRoot -Recurse -Force -ErrorAction SilentlyContinue
  Remove-Item -Path $registryRoot -Recurse -Force -ErrorAction SilentlyContinue
}
