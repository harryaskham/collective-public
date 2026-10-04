# Windows PowerShell 5.1 and PowerShell 7, no admin, no live tasks/runners.
# All programs/files are confined to a unique temp directory, removed on exit.
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
. (Join-Path $PSScriptRoot '..\headless.ps1')

function Assert-True($Description, $Condition) {
  if (-not $Condition) { throw "FAIL: $Description" }
  Write-Host "ok - $Description"
}

$testRoot = Join-Path ([IO.Path]::GetTempPath()) ('collective headless & (test) ' + [guid]::NewGuid().ToString('N'))
$processes = @()
try {
  New-Item -ItemType Directory -Path $testRoot | Out-Null
  $source = Join-Path $PSScriptRoot '..\collective-headless.cs'
  $launcher = Install-CollectiveHeadlessLauncher -SourcePath $source -Directory $testRoot
  $stamp = (Get-Item -LiteralPath $launcher).LastWriteTimeUtc.Ticks
  Assert-True 'install is idempotent' ((Install-CollectiveHeadlessLauncher -SourcePath $source -Directory $testRoot) -eq $launcher)
  Assert-True 'unchanged binary is not rewritten' ((Get-Item -LiteralPath $launcher).LastWriteTimeUtc.Ticks -eq $stamp)
  $bytes = [IO.File]::ReadAllBytes($launcher)
  $pe = [BitConverter]::ToInt32($bytes, 0x3c)
  Assert-True 'launcher uses GUI subsystem (no initial console)' ([BitConverter]::ToUInt16($bytes, $pe + 24 + 68) -eq 2)

  # Probe both the actual thread desktop and a visible GUI window. A separately
  # spawned console child must inherit the background desktop as well.
  $probeSource = @'
using System;
using System.IO;
using System.Text;
using System.Diagnostics;
using System.Runtime.InteropServices;
public static class DesktopProbe {
  [DllImport("kernel32.dll")] static extern uint GetCurrentThreadId();
  [DllImport("user32.dll")] static extern IntPtr GetThreadDesktop(uint id);
  [DllImport("user32.dll", SetLastError=true)] static extern IntPtr OpenInputDesktop(uint flags, bool inherit, uint access);
  [DllImport("user32.dll")] static extern bool CloseDesktop(IntPtr h);
  [DllImport("user32.dll", CharSet=CharSet.Unicode)] static extern bool GetUserObjectInformation(IntPtr h, int index, StringBuilder value, int size, out int needed);
  [DllImport("user32.dll", CharSet=CharSet.Unicode)] static extern IntPtr CreateWindowEx(uint ex, string cls, string title, uint style, int x, int y, int w, int h, IntPtr parent, IntPtr menu, IntPtr instance, IntPtr param);
  [DllImport("user32.dll")] static extern bool DestroyWindow(IntPtr h);
  [DllImport("user32.dll")] static extern bool IsWindowVisible(IntPtr h);
  [DllImport("user32.dll")] static extern bool ShowWindow(IntPtr h, int command);
  static string Name(IntPtr handle) {
    int needed; var value = new StringBuilder(256);
    if (!GetUserObjectInformation(handle, 2, value, 512, out needed)) throw new Exception("Cannot inspect desktop");
    return value.ToString();
  }
  public static string InputDesktop() {
    var handle = OpenInputDesktop(0, false, 1);
    if (handle == IntPtr.Zero) throw new Exception("Cannot inspect input desktop");
    try { return Name(handle); } finally { CloseDesktop(handle); }
  }
  public static int Main(string[] args) {
    if (args[0] == "child") { File.WriteAllText(args[1], Name(GetThreadDesktop(GetCurrentThreadId()))); return 0; }
    var window = CreateWindowEx(0, "STATIC", "Collective isolated test", 0x10000000, 0, 0, 80, 40, IntPtr.Zero, IntPtr.Zero, IntPtr.Zero, IntPtr.Zero);
    try {
      // Explicitly override inherited SW_HIDE, as a GUI app is allowed to do.
      ShowWindow(window, 5); ShowWindow(window, 5);
      File.WriteAllLines("probe.txt", new [] { Name(GetThreadDesktop(GetCurrentThreadId())), Environment.CurrentDirectory, IsWindowVisible(window).ToString(), args.Length.ToString(), System.Security.Principal.WindowsIdentity.GetCurrent().Name });
      File.WriteAllLines("arguments.txt", args);
      var info = new ProcessStartInfo(Process.GetCurrentProcess().MainModule.FileName, "child child.txt");
      info.UseShellExecute = false;
      using (var child = Process.Start(info)) { if (!child.WaitForExit(10000)) { child.Kill(); return 99; } }
      System.Threading.Thread.Sleep(1000);
      return 23;
    } finally { DestroyWindow(window); }
  }
}
'@
  $probe = Join-Path $testRoot 'probe with spaces.exe'
  # PowerShell 7 Add-Type cannot emit an executable. Use the same inbox
  # Framework compiler as production, but make this fixture a console app.
  $probeFile = Join-Path $testRoot 'probe.cs'
  [IO.File]::WriteAllText($probeFile, $probeSource)
  $compiler = Join-Path $env:WINDIR 'Microsoft.NET\Framework64\v4.0.30319\csc.exe'
  if (-not (Test-Path $compiler)) { $compiler = Join-Path $env:WINDIR 'Microsoft.NET\Framework\v4.0.30319\csc.exe' }
  $compileInfo = New-Object Diagnostics.ProcessStartInfo
  $compileInfo.FileName = $compiler
  $compileInfo.Arguments = '/nologo /target:exe /out:' + (ConvertTo-CollectiveWindowsArgument $probe) + ' ' + (ConvertTo-CollectiveWindowsArgument $probeFile)
  $compileInfo.UseShellExecute = $false
  $compileInfo.CreateNoWindow = $true
  $compile = [Diagnostics.Process]::Start($compileInfo)
  $processes += $compile
  Assert-True 'console fixture compiles' ($compile.WaitForExit(20000) -and $compile.ExitCode -eq 0)
  Add-Type -TypeDefinition $probeSource
  $inputDesktop = [DesktopProbe]::InputDesktop()
  $expectedArguments = @('space here', 'quote"here', '', 'trailing\', 'slash\"quote', '&literal%value!')
  $arguments = Get-CollectiveHeadlessArguments -WorkingDirectory $testRoot -FilePath $probe -ArgumentList $expectedArguments
  $p = Start-Process -FilePath $launcher -ArgumentList $arguments -PassThru
  $processes += $p
  Assert-True 'native launch completes within 20 seconds' ($p.WaitForExit(20000))
  Assert-True 'child exit code is propagated' ($p.ExitCode -eq 23)
  $receipt = [IO.File]::ReadAllLines((Join-Path $testRoot 'probe.txt'))
  Assert-True 'actual child desktop is non-visible' ($receipt[0] -eq 'CollectiveBackground' -and $receipt[0] -ne $inputDesktop)
  Assert-True 'working directory containing spaces and metacharacters is preserved' ($receipt[1] -eq $testRoot)
  Assert-True 'Windows account is unchanged' ($receipt[4] -eq [Security.Principal.WindowsIdentity]::GetCurrent().Name)
  Assert-True 'GUI window exists only on background desktop' ($receipt[2] -eq 'True')
  Assert-True 'nested console child inherits background desktop' ([IO.File]::ReadAllText((Join-Path $testRoot 'child.txt')) -eq 'CollectiveBackground')
  Assert-True 'input desktop never changes' ([DesktopProbe]::InputDesktop() -eq $inputDesktop)
  $actualArguments = [IO.File]::ReadAllLines((Join-Path $testRoot 'arguments.txt'))
  Assert-True 'argument count preserved' ($actualArguments.Count -eq $expectedArguments.Count)
  for ($i = 0; $i -lt $expectedArguments.Count; $i++) {
    Assert-True "literal argument $i roundtrips" ($actualArguments[$i] -ceq $expectedArguments[$i])
  }

  $batch = Join-Path $testRoot 'fake run.cmd'
  Set-Content -LiteralPath $batch -Encoding ASCII -Value @('@echo off', 'echo batch>batch.txt', 'exit /b 17')
  $batchArgs = Get-CollectiveHeadlessArguments -WorkingDirectory $testRoot -FilePath $batch
  $p = Start-Process -FilePath $launcher -ArgumentList $batchArgs -PassThru
  $processes += $p
  Assert-True 'batch launch completes' ($p.WaitForExit(10000))
  Assert-True 'quoted batch path runs and preserves exit code' ((Test-Path (Join-Path $testRoot 'batch.txt')) -and $p.ExitCode -eq 17)

  # A missing target must fail closed, with a bounded diagnostic, not fall back
  # to a visible PowerShell/cmd window or a GUI error dialog.
  $missingArgs = Get-CollectiveHeadlessArguments -WorkingDirectory $testRoot -FilePath (Join-Path $testRoot 'missing.exe')
  $p = Start-Process -FilePath $launcher -ArgumentList $missingArgs -PassThru
  $processes += $p
  Assert-True 'missing target exits' ($p.WaitForExit(10000))
  Assert-True 'missing target returns error' ($p.ExitCode -eq 1)
  Assert-True 'launch failure is observable' (Test-Path (Join-Path $testRoot 'collective-headless-error.log'))
} finally {
  foreach ($p in $processes) { if (-not $p.HasExited) { $p.Kill(); $p.WaitForExit() }; $p.Dispose() }
  Remove-Item -LiteralPath $testRoot -Recurse -Force -ErrorAction SilentlyContinue
}
Write-Host 'PASS: headless Windows process boundary'
