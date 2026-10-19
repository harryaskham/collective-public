"""Figures and generated tables for the collective-public site. Tables read scripts/site/data/*.json,
which capture.py records by running the library (tests, examples) and reading the tree (sizes)."""
import html
import json
from pathlib import Path

from diagrams import graph

HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
METRICS = {"sans": 1.0, "mono": 0.6}

ABOUT = {  # one plain line per library file, from its own header or contents
    "typelib.nix": "a type system for Nix values, outside the module system",
    "log.nix": "structured logging and tracing, aware of the type system",
    "functions.nix": "missing functional basics and variadic functions",
    "strings/strings.nix": "string formatting and multi-line indentation",
    "attrsets.nix": "attribute-set helpers",
    "tests.nix": "the test framework: suites, expectations, runners",
    "script-utils/ansi-utils.nix": "terminal colours and styles for generated scripts",
    "colors.nix": "colour schemes and hex and RGB formatting for configs",
    "script-utils/log-utils.nix": "logging for generated shell scripts",
    "dispatchlib.nix": "dispatch on a value's type or shape",
    "nixlike.el": "Emacs: evaluate Nix on the fly in a Nix or Tvix REPL",
    "wm.nix": "window-management intents and key bindings, independent of the window manager",
    "lists.nix": "list helpers",
    "collections.nix": "functions shared by lists, attribute sets and strings",
    "script-utils/options-utils.nix": "command-line option parsing for generated scripts",
    "script-utils/script-types.nix": "builds a script description into the packages it produces",
}


def j(name):
    p = DATA / f"{name}.json"
    return json.loads(p.read_text()) if p.is_file() else []


def levels():
    return graph(
        "levels", "How the type system bootstraps itself",
        "The bootstrap of collective-lib's type system, as typelib.nix describes it. A minimal HyperType, built by hand, "
        "defines what fields a type has. MetaType is a subtype of HyperType and also an instance of it, and adds methods. "
        "ProtoType is a subtype and an instance of MetaType, and binds methods when instantiated. Type is a subtype and "
        "instance of ProtoType and is the base of every other type, from Int and Bool to types you declare.",
        940, 170,
        nodes=[
            dict(id="h", x=90, y=95, label="HyperType", sub="built by hand", kind="muted", w=150, mono=True),
            dict(id="m", x=340, y=95, label="MetaType", sub="adds methods", kind="b", w=150, mono=True),
            dict(id="p", x=590, y=95, label="ProtoType", sub="binds methods", kind="c", w=150, mono=True),
            dict(id="t", x=835, y=95, label="Type", sub=["base of every type", "Int · Bool · yours"], kind="accent", w=180, mono=True),
        ],
        edges=[
            {"from": "h", "to": "m", "kind": "ink"},
            {"from": "m", "to": "p", "kind": "ink"},
            {"from": "p", "to": "t", "kind": "ink"},
        ],
    )


def mirror():
    return graph(
        "mirror", "Where this repository comes from",
        "How collective-public is published. It is a folder, collective-public/, inside a private configuration "
        "repository. A git pre-push hook there notices pushes that touch the folder and makes a commit on the public "
        "repository whose tree is exactly the folder and whose parent is the public repository's current tip, then "
        "fast-forwards it. Other projects take it as a flake input: nix-reflect builds on collective-lib and its test "
        "framework, and the private configuration uses all of it.",
        940, 250,
        nodes=[
            dict(id="priv", x=130, y=80, label="private configuration", sub="collective-public/ folder", kind="muted", w=220),
            dict(id="hook", x=410, y=80, label="pre-push hook", sub="same tree, fast-forward", kind="ink", w=200),
            dict(id="pub", x=690, y=80, label="collective-public", sub="this repository", kind="accent", w=200),
            dict(id="nr", x=560, y=200, label="nix-reflect", sub="collective-lib + tests", kind="b", w=190),
            dict(id="cfg", x=830, y=200, label="the configuration", sub="modules · library", kind="b", w=190),
        ],
        edges=[
            {"from": "priv", "to": "hook", "kind": "ink"},
            {"from": "hook", "to": "pub", "kind": "accent"},
            {"from": "pub", "to": "nr", "kind": "b", "out": "b", "in": "t"},
            {"from": "pub", "to": "cfg", "kind": "b", "out": "b", "in": "t"},
        ],
    )


def tests():
    rows, tp, tt = [], 0, 0
    for r in sorted(j("tests"), key=lambda r: r["module"]):
        total, passed = r.get("total") or 0, r.get("passed", 0)
        failed, skipped = r.get("failed", 0), r.get("skipped", 0)
        tp += passed; tt += total
        state = "all pass" if r["ok"] and failed == 0 and passed == total else (
            f"{failed} failing" + (f", {skipped} skipped" if skipped else "") if r["ok"] else "did not finish")
        cls = "ok" if state == "all pass" else "bad"
        rows.append(f'<tr><th scope="row"><code>{html.escape(r["module"])}</code></th><td class="num">{passed}</td>'
                    f'<td class="num">{total}</td><td class="{cls}">{html.escape(state)}</td></tr>')
    return ('<div class="table-wrap"><table class="tests"><caption>Each module\'s own suite, run one module at a time by '
            f'<code>capture.py</code>: {tp} of {tt} tests pass</caption><thead><tr><th scope="col">Module</th>'
            '<th scope="col">Passed</th><th scope="col">Tests</th><th scope="col">Result</th></tr></thead>'
            f'<tbody>{"".join(rows)}</tbody></table></div>')


def sizes():
    rows = sorted((r for r in j("sizes") if not r["path"].endswith("_test.nix")), key=lambda r: -r["lines"])
    total = sum(r["lines"] for r in rows)
    top = "".join(f'<tr><th scope="row"><code>{html.escape(r["path"])}</code></th><td class="num">{r["lines"]:,}</td>'
                  f'<td>{html.escape(ABOUT.get(r["path"], ""))}</td></tr>' for r in rows[:12])
    return ('<div class="table-wrap"><table><caption>The largest of collective-lib\'s '
            f'{len(rows)} files ({total:,} lines in all), counted when the page was built</caption><thead><tr>'
            '<th scope="col">File</th><th scope="col">Lines</th><th scope="col">What it is</th></tr></thead>'
            f'<tbody>{top}</tbody></table></div>')


def example(name):
    e = next((x for x in j("examples") if x["name"] == name), None)
    if not e:
        return ""
    out = e["out"] if e["ok"] else "error: " + e["err"].removeprefix("error:").strip()
    return (f'<pre class="nix"><code>{html.escape(e["src"])}</code></pre>'
            f'<pre class="out" aria-label="Result"><code>{html.escape(out)}</code></pre>')


FIGS = {"levels": levels, "mirror": mirror}
GEN = {"tests": tests, "sizes": sizes,
       **{f"ex-{n}": (lambda n=n: example(n)) for n in ("construct", "wrong-field", "typeof-raw", "typeof-wrapped", "int-rejects-string")}}
