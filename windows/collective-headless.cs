// Small Windows-only launch adapter, compiled by headless.ps1 with the inbox
// .NET Framework compiler. /target:winexe is essential: the adapter itself must
// not allocate a console. No SDK download, elevation, service or password store.
using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.InteropServices;
using System.Security.Cryptography;
using System.Text;
using System.Threading;

internal static class CollectiveHeadless
{
    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
    private struct StartupInfo
    {
        public int cb;
        public string reserved, desktop, title;
        public int x, y, xSize, ySize, xChars, yChars, fill, flags;
        public short showWindow, reservedSize;
        public IntPtr reserved2, stdin, stdout, stderr;
    }

    [StructLayout(LayoutKind.Sequential)]
    private struct ProcessInformation
    {
        public IntPtr process, thread;
        public uint processId, threadId;
    }

    [DllImport("user32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
    private static extern IntPtr CreateDesktop(string name, IntPtr device,
        IntPtr mode, uint flags, uint access, IntPtr attributes);
    [DllImport("user32.dll", SetLastError = true)]
    private static extern bool CloseDesktop(IntPtr desktop);
    [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
    private static extern bool CreateProcess(string application, StringBuilder command,
        IntPtr processAttributes, IntPtr threadAttributes, bool inheritHandles,
        uint flags, IntPtr environment, string directory, ref StartupInfo startup,
        out ProcessInformation process);
    [DllImport("kernel32.dll", SetLastError = true)]
    private static extern uint WaitForSingleObject(IntPtr handle, uint milliseconds);
    [DllImport("kernel32.dll", SetLastError = true)]
    private static extern bool GetExitCodeProcess(IntPtr process, out uint code);
    [DllImport("kernel32.dll")]
    private static extern bool CloseHandle(IntPtr handle);

    // CommandLineToArgvW/CRT quoting; no shell expansion. cmd.exe /c callers
    // supply its command string separately, rather than passing it through CRT
    // escaping (cmd uses doubled outer quotes, not backslash-escaped quotes).
    private static string Quote(string value)
    {
        var result = new StringBuilder("\"");
        int slashes = 0;
        foreach (char c in value)
        {
            if (c == '\\') { slashes++; continue; }
            result.Append('\\', c == '"' ? slashes * 2 + 1 : slashes);
            result.Append(c);
            slashes = 0;
        }
        result.Append('\\', slashes * 2);
        return result.Append('"').ToString();
    }

    [STAThread]
    private static int Main(string[] args)
    {
        string directory = Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData);
        try
        {
            // Fixed contract: cwd, executable, then its argument vector. A .cmd
            // is the one special case, wrapped with cmd /d /s /c by this adapter.
            if (args.Length < 2) throw new ArgumentException("Expected working-directory, executable, [arguments...]");
            directory = Path.GetFullPath(args[0]);
            string application = Path.GetFullPath(args[1]);
            if (!Directory.Exists(directory)) throw new DirectoryNotFoundException("Working directory does not exist");
            if (!File.Exists(application)) throw new FileNotFoundException("Executable does not exist");
            var command = new StringBuilder(Quote(application));
            bool batch = String.Equals(Path.GetExtension(application), ".cmd", StringComparison.OrdinalIgnoreCase);
            if (batch)
            {
                // Managed run.cmd takes no arguments. Avoid cmd expansion of
                // arbitrary caller input; native .exe launches accept argv.
                if (args.Length != 2 || application.Contains("%"))
                    throw new ArgumentException("Batch launch requires a literal path and no arguments");
                application = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.System), "cmd.exe");
                command = new StringBuilder(Quote(application) + " /d /s /c \"" + command + "\"");
            }
            else
                for (int i = 2; i < args.Length; i++) command.Append(" ").Append(Quote(args[i]));

            string identity;
            using (var hash = SHA256.Create())
                identity = BitConverter.ToString(hash.ComputeHash(Encoding.UTF8.GetBytes(directory + "\n" + command))).Replace("-", "");
            // Same invocation is a singleton, including across logon sessions.
            // A mutex (unlike global file mappings) needs no elevation.
            using (var mutex = new Mutex(false, "Global\\CollectiveHeadless-" + identity))
            {
                bool owned;
                // Allow the prior wrapper a brief reap window after convergence
                // stops a helper; otherwise a restart can race its old mutex.
                try { owned = mutex.WaitOne(3000); }
                catch (AbandonedMutexException) { owned = true; }
                if (!owned) return 0;
                try { return Run(application, command, directory); }
                finally { mutex.ReleaseMutex(); }
            }
        }
        catch (Exception error)
        {
            // One bounded last-error record, no argv/environment/credentials.
            // A GUI-subsystem program must never show an error MessageBox.
            try { File.WriteAllText(Path.Combine(directory, "collective-headless-error.log"),
                DateTime.UtcNow.ToString("o") + " " + error.GetType().Name + ": " + error.Message + Environment.NewLine); }
            catch { }
            return 1;
        }
    }

    private static int Run(string application, StringBuilder command, string directory)
    {
        // Shared per window station, not one desktop heap per repo/timer tick.
        // NEVER SwitchDesktop/SetThreadDesktop: do not change the user's view.
        // Children inherit this desktop even if they allocate another console.
        IntPtr desktop = CreateDesktop("CollectiveBackground", IntPtr.Zero, IntPtr.Zero,
            0, 0x10000000 /* GENERIC_ALL, current user's desktop */, IntPtr.Zero);
        if (desktop == IntPtr.Zero) throw new Win32Exception(Marshal.GetLastWin32Error());
        try
        {
            var startup = new StartupInfo();
            startup.cb = Marshal.SizeOf(startup);
            startup.desktop = "CollectiveBackground";
            startup.flags = 1; // STARTF_USESHOWWINDOW
            startup.showWindow = 0; // SW_HIDE; desktop isolation is the real guarantee
            ProcessInformation child;
            if (!CreateProcess(application, command, IntPtr.Zero, IntPtr.Zero, false,
                0x10 /* CREATE_NEW_CONSOLE: inherited console stays off-screen */,
                IntPtr.Zero, directory, ref startup, out child))
                throw new Win32Exception(Marshal.GetLastWin32Error());
            CloseHandle(child.thread);
            try
            {
                if (WaitForSingleObject(child.process, UInt32.MaxValue) == UInt32.MaxValue)
                    throw new Win32Exception(Marshal.GetLastWin32Error());
                uint code;
                if (!GetExitCodeProcess(child.process, out code))
                    throw new Win32Exception(Marshal.GetLastWin32Error());
                return unchecked((int)code);
            }
            finally { CloseHandle(child.process); }
        }
        finally { CloseDesktop(desktop); }
    }
}
