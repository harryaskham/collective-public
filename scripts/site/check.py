#!/usr/bin/env python3
"""Validate site/ before it is published.

    python3 scripts/site/check.py

Fails (exit 1) when generated regions are stale; the tree has symlinks, unexpected file types,
or files/totals over budget; a page lacks a title, description, theme colour, colour scheme,
skip link or main landmark; a local link or #fragment does not resolve, a URL is root-relative
(it would break under /<repo>/), or an asset loads from another origin; or public text mentions
private infrastructure or local paths.
"""
from __future__ import annotations

import html.parser
import re
import subprocess
import sys
from pathlib import Path
from urllib.parse import unquote, urlsplit

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
SITE = ROOT / "site"
ALLOWED = {".html", ".css", ".js", ".svg", ".woff2", ".txt", ".webp", ".png", ".jpg", ".json", ".m4a", ".ico"}
FILE_BUDGET = {".woff2": 80_000, ".png": 400_000, ".webp": 400_000, ".jpg": 400_000, ".m4a": 400_000, ".json": 2_000_000}
DEFAULT_BUDGET = 300_000
TOTAL_BUDGET = 12_000_000
PRIVATE = [
    (re.compile(r"\.ts\.net\b"), "a tailnet hostname"),
    (re.compile(r"\b100\.(6[4-9]|[7-9]\d|1[01]\d|12[0-7])\.\d{1,3}\.\d{1,3}\b"), "a tailnet (CGNAT) address"),
    (re.compile(r"(?<![\w.])(/home/[a-z]|/Users/[A-Za-z])"), "a local home path"),
    (re.compile(r"\b(helsinki|ms-dev(-\d)?|ms-mac|pocket4|miku-owl)\b"), "a private host"),
    (re.compile(r"microsoft\.ghe\.com|msft\.ghe\.com"), "an enterprise host"),
    (re.compile(r"(?i)\b(ghp|gho|ghu|github_pat)_[A-Za-z0-9_]{10,}"), "a token"),
]


class Page(html.parser.HTMLParser):
    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.ids: list[str] = []
        self.refs: list[tuple[str, str, str]] = []
        self.meta: dict[str, str] = {}
        self.title = self.main = self.skip = False

    def handle_starttag(self, tag, attrs):
        a = dict(attrs)
        if "id" in a:
            self.ids.append(a["id"])
        for attr in ("href", "src", "poster"):
            if a.get(attr):
                self.refs.append((tag, attr, a[attr]))
        if tag == "meta" and a.get("name"):
            self.meta[a["name"]] = a.get("content", "")
        self.title |= tag == "title"
        self.main |= tag == "main" and a.get("id") == "main"
        self.skip |= tag == "a" and "skip" in (a.get("class") or "").split()


def main() -> int:
    bad: list[str] = []
    for script in ("data.py", "build.py", "subset-font.py"):
        if (HERE / script).exists():
            r = subprocess.run([sys.executable, str(HERE / script), "--check"], capture_output=True, text=True)
            if r.returncode:
                bad.append((r.stdout + r.stderr).strip())
    total = 0
    pages: dict[Path, Page] = {}
    if not (SITE / "index.html").is_file():
        bad.append("site/index.html is missing")
    for f in sorted(SITE.rglob("*")):
        rel = f.relative_to(SITE)
        if f.is_symlink():
            bad.append(f"{rel}: symlink")
            continue
        if f.is_dir():
            continue
        if f.suffix not in ALLOWED:
            bad.append(f"{rel}: unexpected file type")
        size = f.stat().st_size
        total += size
        if size > FILE_BUDGET.get(f.suffix, DEFAULT_BUDGET):
            bad.append(f"{rel}: {size} bytes, over budget")
        if f.suffix in (".html", ".css", ".js", ".svg", ".txt", ".json"):
            text = f.read_text()
            for pat, what in PRIVATE:
                m = pat.search(text)
                if m:
                    bad.append(f"{rel}: mentions {what} ({m.group(0)!r})")
            if f.suffix == ".css":
                for m in re.finditer(r"url\((['\"]?)([^)'\"]+)\1\)", text):
                    if m.group(2).startswith(("http:", "https:", "//")):
                        bad.append(f"{rel}: loads {m.group(2)} from another origin")
        if f.suffix == ".html":
            p = Page()
            p.feed(f.read_text())
            pages[f] = p
    if total > TOTAL_BUDGET:
        bad.append(f"site/: {total} bytes in total, over {TOTAL_BUDGET}")

    for f, p in pages.items():
        rel = f.relative_to(SITE)
        dup = sorted({i for i in p.ids if p.ids.count(i) > 1})
        if dup:
            bad.append(f"{rel}: duplicate ids {dup}")
        for need in ("description", "theme-color", "color-scheme"):
            if not p.meta.get(need):
                bad.append(f"{rel}: missing <meta name={need}>")
        for ok, what in ((p.title, "<title>"), (p.main, "<main id=main>"), (p.skip, "a skip link")):
            if not ok:
                bad.append(f"{rel}: missing {what}")
        for tag, attr, url in p.refs:
            if url.startswith(("data:", "mailto:", "javascript:")):
                continue
            parts = urlsplit(url)
            if parts.scheme in ("http", "https") or url.startswith("//"):
                if tag in ("script", "img", "audio", "video", "source", "iframe") or (tag == "link" and attr == "href"):
                    bad.append(f"{rel}: <{tag} {attr}> loads {url} from another origin")
                continue
            if url.startswith("/"):
                bad.append(f"{rel}: root-relative URL {url}")
                continue
            target = f if not parts.path else (f.parent / unquote(parts.path)).resolve()
            if parts.path:
                try:
                    target.relative_to(SITE.resolve())
                except ValueError:
                    bad.append(f"{rel}: {url} leaves the site")
                    continue
                if target.is_dir():
                    target = target / "index.html"
                if not target.exists():
                    bad.append(f"{rel}: {url} does not exist")
                    continue
            frag = parts.fragment
            if frag and target.suffix == ".html" and not frag.startswith("/"):
                tp = pages.get(target)
                if tp is None:
                    tp = Page()
                    tp.feed(target.read_text())
                if frag not in tp.ids:
                    bad.append(f"{rel}: {url} has no #{frag} target")

    for b in bad:
        print(f"check: {b}", file=sys.stderr)
    print(f"check: {len(pages)} pages, {total / 1e6:.2f} MB, {len(bad)} problem(s)")
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
