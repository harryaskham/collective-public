#!/usr/bin/env python3
"""Record what the site quotes by running collective-lib itself.

    python3 scripts/site/capture.py            # everything (the module test suites take a while)
    python3 scripts/site/capture.py examples   # only the type-system examples
    python3 scripts/site/capture.py sizes      # only the module sizes

Writes scripts/site/data/{examples,tests,sizes}.json for build.py. Every evaluation runs as
`nix eval` against this flake's lib output with env -i and a scratch HOME, so no personal
configuration leaks in. Test suites run one module at a time with tracing off.
"""
import json
import os
import re
import subprocess
import sys
import tempfile
import time
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
DATA = HERE / "data"
LIB = ROOT / "pkgs" / "collective-lib"

# Each example: (name, Nix source applied to the flake's lib, what to show the reader).
EXAMPLES = [
    ("construct", '''with lib.typed;
let A = Type "A" { fields = [ { a = "int"; } { b = Int; } { c = Default Int 5; } ]; };
    x = A 1 (Int 2);
in [ x.a x.b.value x.c.value ]'''),
    ("wrong-field", '''with lib.typed;
let A = Type "A" { fields = [ { a = "int"; } { b = Int; } ]; };
in (A 1 "two").b.value'''),
    ("typeof-raw", '''with lib.typed; typeOf true'''),
    ("typeof-wrapped", '''with lib.typed; typeOf (Bool true) == Bool'''),
    ("int-rejects-string", '''with lib.typed; (Int "123").value'''),
]


def env():
    e = {"HOME": tempfile.mkdtemp(prefix="cp-home-"), "PATH": os.environ["PATH"], "CLTV_TRACE_LEVEL": "0",
         "NIX_CONFIG": "experimental-features = nix-command flakes"}
    return e


def nix_eval(apply: str, raw=False, timeout=1800):
    args = ["nix", "eval", "--impure", "--option", "max-call-depth", "1000000", "--raw" if raw else "--json",
            "--apply", apply, ".#lib.x86_64-linux"]
    t = time.time()
    r = subprocess.run(args, cwd=ROOT, env=env(), capture_output=True, text=True, encoding="utf-8", errors="replace", timeout=timeout)
    err = [l for l in r.stderr.splitlines() if l.strip() and not l.startswith(("warning:", "trace:"))]
    msg = next((l.strip() for l in reversed(err) if "error:" in l), err[-1].strip() if err else "")
    return {"ok": r.returncode == 0, "out": r.stdout.strip(), "err": msg[:300], "s": round(time.time() - t, 1)}


def examples():
    out = []
    for name, src in EXAMPLES:
        r = nix_eval(f"lib: ({src})")
        out.append({"name": name, "src": src, **r})
        print(f"{name:20s} {'ok ' + r['out'][:40] if r['ok'] else 'error ' + r['err'][:60]}", flush=True)
    (DATA / "examples.json").write_text(json.dumps(out, indent=1) + "\n")


def modules_with_tests():
    r = nix_eval('lib: builtins.filter (n: (lib.base.${n} or {}) ? _tests) (builtins.attrNames lib.base)')
    return [m for m in json.loads(r["out"]) if m != "nix-reflect"]  # nix-reflect's suites belong to that project


def run_module(m):
    r = nix_eval(f'lib: let r = lib.base."{m}"._tests.run {{}}; in if builtins.isString r then r else builtins.toJSON r',
                 raw=True)
    text = re.sub(r"\x1b\[[0-9;]*m|\\e\[[0-9;]*m", "", r["out"])
    counts = {k.lower(): int(n) for n, k in re.findall(r"(\d+) of \d+ tests (passed|failed|skipped)", text)}
    total = re.search(r"Running (\d+) tests", text)
    failed = re.findall(r"^FAIL: (\S+)", text, re.M)
    return {"module": m, "ok": r["ok"], "total": int(total.group(1)) if total else None, **counts,
            "failed_tests": failed[:40], "err": "" if r["ok"] else r["err"], "s": r["s"]}


def tests():
    mods = modules_with_tests()
    with ThreadPoolExecutor(4) as pool:
        res = list(pool.map(run_module, mods))
    for r in res:
        print(f"{r['module']:14s} total={r['total']} passed={r.get('passed')} failed={r.get('failed')} ok={r['ok']} {r['s']}s", flush=True)
    (DATA / "tests.json").write_text(json.dumps(res, indent=1) + "\n")


def sizes():
    rows = []
    for f in sorted(LIB.rglob("*")):
        if f.is_file() and f.suffix in (".nix", ".el", ".sh"):
            rows.append({"path": str(f.relative_to(LIB)), "lines": sum(1 for _ in f.open(errors="replace"))})
    (DATA / "sizes.json").write_text(json.dumps(rows, indent=1) + "\n")
    print(f"sizes: {len(rows)} files, {sum(r['lines'] for r in rows)} lines")


def main():
    DATA.mkdir(exist_ok=True)
    which = set(sys.argv[1:]) or {"examples", "tests", "sizes"}
    if "sizes" in which: sizes()
    if "examples" in which: examples()
    if "tests" in which: tests()


if __name__ == "__main__":
    main()
