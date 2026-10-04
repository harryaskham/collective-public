# Shared by Collective's Windows runner and devbox modules; safe to dot-source.
# Keep Windows PowerShell 5.1 compatibility. No task registration or launch on load.

function ConvertTo-CollectiveWindowsArgument {
  param([AllowEmptyString()][Parameter(Mandatory = $true)][string]$Value)
  # Windows CRT quoting, including quotes and trailing backslashes.
  return '"' + ([regex]::Replace(
    ([regex]::Replace($Value, '(\\*)"', '$1$1\"')), '(\\+)$', '$1$1')) + '"'
}

function Get-CollectiveHeadlessArguments {
  param(
    [Parameter(Mandatory = $true)][string]$WorkingDirectory,
    [Parameter(Mandatory = $true)][string]$FilePath,
    [string[]]$ArgumentList = @()
  )
  return ((@($WorkingDirectory, $FilePath) + $ArgumentList | ForEach-Object {
    ConvertTo-CollectiveWindowsArgument -Value $_
  }) -join ' ')
}

function Install-CollectiveHeadlessLauncher {
  [CmdletBinding()]
  param(
    [Parameter(Mandatory = $true)][string]$SourcePath,
    [string]$Directory = (Join-Path $env:LOCALAPPDATA 'Collective\headless')
  )
  $hash = (Get-FileHash -LiteralPath $SourcePath -Algorithm SHA256).Hash.ToLowerInvariant()
  New-Item -ItemType Directory -Force -Path $Directory | Out-Null
  # Content-addressed binaries can be updated while older launchers are running.
  $exe = Join-Path $Directory ("collective-headless-" + $hash + '.exe')
  if (Test-Path -LiteralPath $exe) { return $exe }
  $compiler = Join-Path $env:WINDIR 'Microsoft.NET\Framework64\v4.0.30319\csc.exe'
  if (-not (Test-Path -LiteralPath $compiler)) {
    $compiler = Join-Path $env:WINDIR 'Microsoft.NET\Framework\v4.0.30319\csc.exe'
  }
  if (-not (Test-Path -LiteralPath $compiler)) { throw 'Windows .NET Framework C# compiler is missing; headless launch unavailable' }

  $temporary = Join-Path $Directory ([guid]::NewGuid().ToString('N') + '.exe')
  $process = New-Object Diagnostics.Process
  try {
    $process.StartInfo.FileName = $compiler
    $process.StartInfo.Arguments = '/nologo /target:winexe /optimize+ /out:' + (ConvertTo-CollectiveWindowsArgument $temporary) + ' ' + (ConvertTo-CollectiveWindowsArgument $SourcePath)
    $process.StartInfo.UseShellExecute = $false
    $process.StartInfo.CreateNoWindow = $true
    $process.StartInfo.RedirectStandardOutput = $true
    $process.StartInfo.RedirectStandardError = $true
    [void]$process.Start()
    $stdout = $process.StandardOutput.ReadToEndAsync()
    $stderr = $process.StandardError.ReadToEndAsync()
    if (-not $process.WaitForExit(60000)) {
      $process.Kill()
      throw 'Headless launcher compilation timed out'
    }
    if ($process.ExitCode -ne 0) { throw "Headless launcher compilation failed: $($stdout.Result) $($stderr.Result)" }
    # Concurrent runner installers may compile the same version. Never replace
    # an already-published/running executable; discard our identical result.
    try { [IO.File]::Move($temporary, $exe) }
    catch { if (-not (Test-Path -LiteralPath $exe)) { throw } }
    return $exe
  } finally {
    $process.Dispose()
    Remove-Item -LiteralPath $temporary -Force -ErrorAction SilentlyContinue
  }
}

function Write-CollectiveHeadlessShortcut {
  [CmdletBinding()]
  param(
    [Parameter(Mandatory = $true)][string]$Path,
    [Parameter(Mandatory = $true)][string]$Launcher,
    [Parameter(Mandatory = $true)][string]$WorkingDirectory,
    [Parameter(Mandatory = $true)][string]$FilePath,
    [string[]]$ArgumentList = @()
  )
  $arguments = Get-CollectiveHeadlessArguments -WorkingDirectory $WorkingDirectory -FilePath $FilePath -ArgumentList $ArgumentList
  $shell = New-Object -ComObject WScript.Shell
  try {
    $shortcut = $shell.CreateShortcut($Path)
    try {
      # No rewriting on every timer tick (nor truncating a running .cmd file).
      if ($shortcut.TargetPath -eq $Launcher -and $shortcut.Arguments -ceq $arguments -and
          $shortcut.WorkingDirectory -eq $WorkingDirectory) { return }
      $shortcut.TargetPath = $Launcher
      $shortcut.Arguments = $arguments
      $shortcut.WorkingDirectory = $WorkingDirectory
      $shortcut.Description = 'Collective background process (non-visible desktop)'
      $shortcut.Save()
    } finally { [void][Runtime.InteropServices.Marshal]::FinalReleaseComObject($shortcut) }
  } finally { [void][Runtime.InteropServices.Marshal]::FinalReleaseComObject($shell) }
}

function Start-CollectiveHeadlessProcess {
  [CmdletBinding()]
  param(
    [Parameter(Mandatory = $true)][string]$Launcher,
    [Parameter(Mandatory = $true)][string]$WorkingDirectory,
    [Parameter(Mandatory = $true)][string]$FilePath,
    [string[]]$ArgumentList = @()
  )
  # ShellExecute detaches the GUI-subsystem adapter from WSL stdio. Framework
  # Process.Start with UseShellExecute=false inherits the installer's pipe
  # handles even without redirection, keeping SSH/systemd output open forever.
  # The target is our exact .exe (not a shell command or file association).
  $info = New-Object Diagnostics.ProcessStartInfo
  $info.FileName = $Launcher
  $info.Arguments = Get-CollectiveHeadlessArguments -WorkingDirectory $WorkingDirectory -FilePath $FilePath -ArgumentList $ArgumentList
  $info.WorkingDirectory = $WorkingDirectory
  $info.UseShellExecute = $true
  $info.ErrorDialog = $false
  $process = [Diagnostics.Process]::Start($info)
  $process.Dispose()
}
