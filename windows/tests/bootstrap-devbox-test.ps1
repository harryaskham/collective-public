# bootstrap-devbox.ps1 placement tests: Windows PowerShell 5.1 and PowerShell 7.
# Loads only the pure functions through the parser; nothing is installed,
# imported or changed.
#   powershell.exe -NoProfile -ExecutionPolicy Bypass -File collective-public/windows/tests/bootstrap-devbox-test.ps1
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

$source = Join-Path $PSScriptRoot '..\bootstrap-devbox.ps1'
$tokens = $null; $errors = $null
$ast = [Management.Automation.Language.Parser]::ParseFile($source, [ref]$tokens, [ref]$errors)
if (@($errors).Count) { throw "bootstrap-devbox.ps1 does not parse: $($errors[0].Message)" }
foreach ($name in @('Select-DevboxVolume', 'Get-DevboxVhdCeiling', 'Get-DevboxVolumes')) {
  $definition = $ast.Find({ param($node) $node -is [Management.Automation.Language.FunctionDefinitionAst] -and $node.Name -eq $name }, $true)
  if (-not $definition) { throw "Missing function $name" }
  . ([scriptblock]::Create($definition.Extent.Text))
}

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
function Volume($Root, $TotalGB, $FreeGB = 100) {
  [pscustomobject]@{ Root = $Root; TotalBytes = [int64]$TotalGB * 1GB; FreeBytes = [int64]$FreeGB * 1GB }
}

# Dev Boxes with a 512 GB C: and a 1.5 TB Q: (ms-dev-0, -1, -a; ms-dev-7/8/9).
$c512 = Volume 'C:\' 512; $q = Volume 'Q:\' 1535
Assert-True 'a larger non-system drive is chosen' ((Select-DevboxVolume -Volumes @($c512, $q) -SystemRoot 'C:\').Root -eq 'Q:\')
# Older Dev Boxes have only a 2 TB C:.
$c2t = Volume 'C:\' 2047
Assert-True 'C: alone stays on C:' ((Select-DevboxVolume -Volumes @($c2t) -SystemRoot 'C:\').Root -eq 'C:\')
Assert-True 'a smaller extra drive is not chosen' ((Select-DevboxVolume -Volumes @($c2t, (Volume 'D:\' 256)) -SystemRoot 'C:\').Root -eq 'C:\')
Assert-True 'an explicit drive wins' ((Select-DevboxVolume -Volumes @($c512, $q) -SystemRoot 'C:\' -Drive 'c').Root -eq 'C:\')
Assert-True 'an explicit lowercase drive is matched' ((Select-DevboxVolume -Volumes @($c512, $q) -SystemRoot 'C:\' -Drive 'q').Root -eq 'Q:\')
Assert-Throws 'a missing explicit drive is refused' { Select-DevboxVolume -Volumes @($c512) -SystemRoot 'C:\' -Drive 'Q' } 'not a ready'

# VHD ceiling: keep the requested size when it fits, else volume minus 50 GB.
Assert-True '1536GB fits a 2 TB C:' ((Get-DevboxVhdCeiling -Requested '1536GB' -VolumeTotalBytes $c2t.TotalBytes) -eq '1536GB')
Assert-True '1536GB is capped on a 1535 GB Q:' ((Get-DevboxVhdCeiling -Requested '1536GB' -VolumeTotalBytes $q.TotalBytes) -eq '1485GB')
Assert-True '1536GB is capped on a 512 GB C:' ((Get-DevboxVhdCeiling -Requested '1536GB' -VolumeTotalBytes $c512.TotalBytes) -eq '462GB')
Assert-True '1TB that fits is kept as written' ((Get-DevboxVhdCeiling -Requested '1TB' -VolumeTotalBytes $q.TotalBytes) -eq '1TB')
Assert-Throws 'a tiny volume is refused' { Get-DevboxVhdCeiling -Requested '1536GB' -VolumeTotalBytes (100GB) } 'too small'
Assert-Throws 'a malformed size is refused' { Get-DevboxVhdCeiling -Requested '1.5TB' -VolumeTotalBytes $q.TotalBytes } 'Invalid'

# The live probe returns ready NTFS/ReFS fixed volumes, including the system drive.
$live = @(Get-DevboxVolumes)
Assert-True 'live volumes include the system drive' (@($live | Where-Object { $_.Root -ieq [IO.Path]::GetPathRoot($env:SystemRoot) }).Count -eq 1)

Write-Host 'PASS: bootstrap-devbox placement and VHD ceiling'
