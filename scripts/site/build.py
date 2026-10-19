#!/usr/bin/env python3
"""Fill generated regions in site/*.html.

    python3 scripts/site/build.py          # rewrite regions in place
    python3 scripts/site/build.py --check  # exit 1 if any page is stale

Regions are HTML comments, so the pages stay plain, reviewable HTML:

    <!-- fig:NAME --> … <!-- /fig -->     an SVG drawing from figures.FIGS[NAME]
    <!-- gen:NAME --> … <!-- /gen -->     markup from figures.GEN[NAME]

figures.py (beside this file) defines FIGS and GEN as {name: callable returning str} and may
set METRICS for the drawing font. Nothing is fetched; output depends only on the repository.
"""
import re
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
SITE = ROOT / "site"
sys.path.insert(0, str(HERE))
import diagrams  # noqa: E402
import figures  # noqa: E402

REGION = re.compile(r"(<!-- (fig|gen):([a-z0-9-]+) -->)(.*?)(<!-- /\2 -->)", re.S)


def render(page: Path, cache: dict) -> str:
    src = page.read_text()

    def fill(m):
        kind, name = m.group(2), m.group(3)
        table = figures.FIGS if kind == "fig" else getattr(figures, "GEN", {})
        if name not in table:
            raise SystemExit(f"{page.relative_to(ROOT)}: no {kind} named {name!r} in figures.py")
        key = (kind, name)
        if key not in cache:
            cache[key] = table[name]().strip()
        return f"{m.group(1)}\n{cache[key]}\n{m.group(5)}"

    return REGION.sub(fill, src)


def main() -> int:
    check = "--check" in sys.argv[1:]
    if hasattr(figures, "METRICS"):
        diagrams.use(**figures.METRICS)
    cache: dict = {}
    stale = []
    for page in sorted(SITE.rglob("*.html")):
        out = render(page, cache)
        if out != page.read_text():
            if check:
                stale.append(str(page.relative_to(ROOT)))
            else:
                page.write_text(out)
    if stale:
        print("stale generated regions: " + ", ".join(stale) + " (run scripts/site/build.py)")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
