"""Deterministic inline-SVG drawings for the tools site.

Each tool page has its own visual language, so drawings carry no colours or fonts: every
stroke and fill is a CSS class (.dg .k-KIND .node, .edge, .lbl …) that the page's style.css
resolves. scripts/site/build.py renders them into the page at <!-- fig:NAME --> markers,
after selecting the page's font metrics with use(), so boxes fit the page's typeface.

Kinds are generic roles a page maps to its palette: ink (default), accent, a, b, c, d, ok,
warn, err, muted, file.

  graph(...)       boxes on explicit coordinates, with frames, orthogonal or curved edges
  sequence(...)    participants with lifelines, numbered messages, notes, frames
  pianoroll(...)   notes on a pitch × time grid from a real `chirp plan`
  waveform(...)    a min/max envelope of real rendered audio
  Canvas           low-level drawing for bespoke sheets
"""

from __future__ import annotations

import html
import math
import re

KINDS = ("ink", "accent", "a", "b", "c", "d", "ok", "warn", "err", "muted", "file")

# Advance-width factors for the page being rendered (see use()).
METRICS = {"sans": 1.0, "mono": 0.6}


def use(sans: float = 1.0, mono: float = 0.6) -> None:
    """Select the current page's font metrics: sans scales the proportional estimate, mono is the
    monospace advance in ems."""
    METRICS["sans"], METRICS["mono"] = sans, mono


def esc(s) -> str:
    return html.escape(str(s), quote=True)


def text_w(s: str, size: float, mono: bool = False) -> float:
    """Conservative advance width of s at size, for the page's fonts (see use())."""
    if mono:
        return len(s) * size * METRICS["mono"]
    w = 0.0
    for ch in s:
        if ch in "il.,:;'|!()[]{} ·/":
            w += 0.30
        elif ch in "mwMW@":
            w += 0.88
        elif ch.isupper() or ch.isdigit():
            w += 0.66
        elif ch in "→←↔⇄✓✕":
            w += 1.0
        else:
            w += 0.55
    return w * size * METRICS["sans"]


def caps_w(s: str, size: float) -> float:
    """Width of a letter-spaced (.1em), narrowed upper-case label (frame tabs, caps)."""
    return sum((0.36 if ch in " ·." else 0.64) + 0.1 for ch in s) * size


def lines_of(v) -> list[str]:
    if v is None:
        return []
    return list(v) if isinstance(v, (list, tuple)) else [v]


class Canvas:
    def __init__(self, name: str, width: float, height: float, title: str, desc: str):
        if not re.fullmatch(r"[a-z0-9-]+", name):
            raise ValueError(f"bad drawing name {name!r}")
        self.p, self.w, self.h, self.title, self.desc = name, width, height, title, desc
        self.defs: dict[str, str] = {}
        self.layers: dict[str, list[str]] = {"under": [], "groups": [], "edges": [], "nodes": [], "labels": [], "over": []}

    def tip(self, kind: str, open_: bool = False) -> str:
        mid = f"{self.p}-tip-{kind}{'-o' if open_ else ''}"
        if mid not in self.defs:
            path = ('<path class="tip open" d="M1.5 1.5L8.5 5L1.5 8.5"/>' if open_ else '<path class="tip" d="M0 1L10 5L0 9z"/>')
            self.defs[mid] = (f'<marker id="{mid}" class="k-{kind}" viewBox="0 0 10 10" refX="9" refY="5" '
                              f'markerWidth="8" markerHeight="8" orient="auto-start-reverse">{path}</marker>')
        return mid

    def hatch(self, kind: str) -> str:
        hid = f"{self.p}-hatch-{kind}"
        if hid not in self.defs:
            self.defs[hid] = (f'<pattern id="{hid}" class="k-{kind}" width="7" height="7" patternUnits="userSpaceOnUse" '
                              f'patternTransform="rotate(45)"><line class="hatch-l" x1="0" y1="0" x2="0" y2="7"/></pattern>')
        return hid

    def add(self, layer: str, svg: str) -> None:
        self.layers[layer].append(svg)

    def svg(self) -> str:
        w, h = math.ceil(self.w), math.ceil(self.h)
        out = [f'<svg class="dg" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {w} {h}" width="{w}" height="{h}" '
               f'role="img" aria-labelledby="{self.p}-t {self.p}-d">',
               f'<title id="{self.p}-t">{esc(self.title)}</title>',
               f'<desc id="{self.p}-d">{esc(self.desc)}</desc>']
        if self.defs:
            out.append("<defs>" + "".join(self.defs[k] for k in sorted(self.defs)) + "</defs>")
        for layer in ("under", "groups", "edges", "nodes", "labels", "over"):
            out += self.layers[layer]
        out.append("</svg>")
        return "\n".join(out)


def T(x, y, s, size=13, cls="sub", anchor="middle", weight=None, mono=False, halo=False, extra="") -> str:
    classes = [cls] + (["m"] if mono else []) + (["halo"] if halo else [])
    wt = f' font-weight="{weight}"' if weight else ""
    return (f'<text x="{x:.1f}" y="{y:.1f}" font-size="{size}" text-anchor="{anchor}" '
            f'class="{" ".join(c for c in classes if c)}"{wt}{extra}>{esc(s)}</text>')


# ------------------------------------------------------------------- glyphs

def store(x: float, y: float, state: str = "open", w: float = 30, h: float = 34, kind: str = "file") -> str:
    """A data store (cylinder). state: open | closed (dashed) | live (a band) | resume (dashed, banded)."""
    rx, ry = w / 2, w * 0.17
    top, bot = y - h / 2, y + h / 2
    dash = state in ("closed", "resume")
    cls = "node" + (" ghost" if dash else "")
    body = (f'<path class="{cls}" d="M{x - rx:.1f} {top:.1f} V{bot:.1f} A{rx:.1f} {ry:.1f} 0 0 0 {x + rx:.1f} {bot:.1f} '
            f'V{top:.1f}"/>')
    lid = f'<ellipse class="{cls}" cx="{x:.1f}" cy="{top:.1f}" rx="{rx:.1f}" ry="{ry:.1f}"/>'
    out = [f'<g class="k-{kind}">']
    if not dash:
        out.append(f'<path class="shadow" d="M{x - rx + 3:.1f} {top + 3:.1f} V{bot + 3:.1f} A{rx:.1f} {ry:.1f} 0 0 0 '
                   f'{x + rx + 3:.1f} {bot + 3:.1f} V{top + 3:.1f}z"/>')
    out += [body, lid]
    if state in ("live", "resume"):
        band_y = y - h * 0.08
        out.append(f'</g><g class="k-accent"><path class="band" d="M{x - rx:.1f} {band_y:.1f} '
                   f'A{rx:.1f} {ry:.1f} 0 0 0 {x + rx:.1f} {band_y:.1f} v6 A{rx:.1f} {ry:.1f} 0 0 1 {x - rx:.1f} '
                   f'{band_y + 6:.1f}z"/>')
    out.append("</g>")
    return "".join(out)


def task(x: float, y: float, state: str, s: float = 13, kind: str = "ink") -> str:
    """A Durable task. state: pending | running | waiting | completing."""
    x0, y0 = x - s / 2, y - s / 2
    out = [f'<g class="k-{kind}">']
    if state == "running":
        out.append(f'<rect class="node solid" x="{x0:.1f}" y="{y0:.1f}" width="{s}" height="{s}"/>')
    elif state == "waiting":
        out.append(f'<rect class="node" x="{x0:.1f}" y="{y0:.1f}" width="{s}" height="{s}"/>')
        out.append(f'<path class="node solid" d="M{x0:.1f} {y0:.1f}h{s / 2:.1f}v{s}h{-s / 2:.1f}z"/>')
    elif state == "completing":
        out.append(f'<rect class="node" x="{x0:.1f}" y="{y0:.1f}" width="{s}" height="{s}"/>')
        out.append(f'<rect class="node solid" x="{x - s * 0.18:.1f}" y="{y - s * 0.18:.1f}" width="{s * 0.36:.1f}" height="{s * 0.36:.1f}"/>')
    else:
        out.append(f'<rect class="node" x="{x0:.1f}" y="{y0:.1f}" width="{s}" height="{s}"/>')
    out.append("</g>")
    return "".join(out)


TAG_KIND = {"queued": "muted", "placed": "ink", "done": "ok", "unanswered": "err"}


def tag(x: float, y: float, text: str, state: str = "placed", mono: bool = True, size: float = 12) -> str:
    """A submission tag (left-anchored at x). state: queued | placed | done | unanswered."""
    tw = text_w(text, size, mono) + 22 + (14 if state in ("done", "unanswered") else 0)
    h = size + 12
    kind = TAG_KIND[state]
    cls = "node" + (" ghost" if state == "queued" else "")
    pts = f"{x:.1f},{y - h / 2:.1f} {x + tw:.1f},{y - h / 2:.1f} {x + tw:.1f},{y + h / 2:.1f} {x:.1f},{y + h / 2:.1f} {x - 8:.1f},{y:.1f}"
    out = [f'<g class="k-{kind}"><polygon class="{cls}" points="{pts}"/>',
           f'<circle class="node solid" cx="{x + 2:.1f}" cy="{y:.1f}" r="2"/>',
           T(x + 10, y + size * 0.36, text, size, "lbl", "start", mono=mono)]
    if state == "done":
        out.append(f'<path class="mark" d="M{x + tw - 18:.1f} {y:.1f}l3.5 3.5l6-7"/>')
    elif state == "unanswered":
        out.append(f'<path class="mark" d="M{x + tw - 17:.1f} {y - 4:.1f}l8 8m0 -8l-8 8"/>')
    out.append("</g>")
    return "".join(out)


def tick(x: float, y: float, h: float = 14, kind: str = "accent") -> str:
    return f'<g class="k-{kind}"><line class="tick" x1="{x:.1f}" y1="{y - h / 2:.1f}" x2="{x:.1f}" y2="{y + h / 2:.1f}"/></g>'


# -------------------------------------------------------------------- graph

class Node:
    def __init__(self, id, x, y, label, sub=None, kind="ink", w=None, h=None, shape="box", mono=False,
                 live=False, ghost=False, tag=None, count=None, state="open", label_pos="below"):
        self.id, self.x, self.y, self.label, self.kind = id, x, y, label, kind
        self.label_pos = label_pos
        self.sub, self.shape, self.mono, self.live, self.ghost = lines_of(sub), shape, mono, live, ghost
        self.tag, self.count, self.state = tag, count, state
        if kind not in KINDS:
            raise ValueError(f"unknown kind {kind!r}")
        if shape in ("dot", "end"):
            self.w = self.h = 14
        elif shape == "store":
            self.w, self.h = w or 34, h or 38
        else:
            lw = text_w(label, 15, mono)
            sw = max((text_w(s, 12.5) for s in self.sub), default=0)
            tw = text_w(tag, 10.5, True) + 18 if tag else 0
            self.w = w or max(92, lw + 32, sw + 28, tw + 24)
            self.h = h or (40 + 16 * len(self.sub) + (14 if tag else 0))

    @property
    def box(self):
        return self.x - self.w / 2, self.y - self.h / 2, self.x + self.w / 2, self.y + self.h / 2


def _anchor(n: Node, tx: float, ty: float, side: str | None = None):
    x0, y0, x1, y1 = n.box
    dx, dy = tx - n.x, ty - n.y
    if side is None:
        side = ("r" if dx > 0 else "l") if abs(dx) * n.h > abs(dy) * n.w else ("b" if dy > 0 else "t")
    pt = {"r": (x1, n.y), "l": (x0, n.y), "b": (n.x, y1), "t": (n.x, y0)}[side]
    vec = {"r": (1, 0), "l": (-1, 0), "b": (0, 1), "t": (0, -1)}[side]
    return side, pt, vec


def _node_svg(c: Canvas, n: Node) -> None:
    g = []
    x0, y0, x1, y1 = n.box
    if n.shape == "dot":
        g.append(f'<rect class="node solid" x="{n.x - 6}" y="{n.y - 6}" width="12" height="12"/>')
    elif n.shape == "end":
        g.append(f'<rect class="node" x="{n.x - 8}" y="{n.y - 8}" width="16" height="16"/>'
                 f'<rect class="node solid" x="{n.x - 4}" y="{n.y - 4}" width="8" height="8"/>')
    elif n.shape == "store":
        c.add("nodes", store(n.x, n.y, n.state, n.w * 0.82, n.h * 0.82, n.kind))
        lines = [(n.label, 13.5, "lbl", n.mono)] + [(s, 12, "sub", False) for s in n.sub]
        if n.label_pos in ("left", "right"):
            ax, anchor = (x0 - 8, "end") if n.label_pos == "left" else (x1 + 8, "start")
            top = n.y - 7 * (len(lines) - 1) + 4
            for i, (t, size, cls, mono) in enumerate(lines):
                g.append(T(ax, top + 14 * i, t, size, cls, anchor, mono=mono, halo=True))
        else:
            first = y1 + 16 if n.label_pos == "below" else y0 - 10 - 14 * (len(lines) - 1)
            for i, (t, size, cls, mono) in enumerate(lines):
                g.append(T(n.x, first + (15 if i else 0) + 14 * max(0, i - 1), t, size, cls, mono=mono, halo=True))
    else:
        if n.shape == "stack":
            for k in (2, 1):
                g.append(f'<rect class="node" x="{x0 + 5 * k:.1f}" y="{y0 - 5 * k:.1f}" width="{n.w:.1f}" height="{n.h:.1f}"/>')
        if n.shape == "diamond":
            pts = f"{n.x},{y0 - 6} {x1 + 10},{n.y} {n.x},{y1 + 6} {x0 - 10},{n.y}"
            g.append(f'<polygon class="shadow" points="{pts}" transform="translate(4 4)"/>')
            g.append(f'<polygon class="node" points="{pts}"/>')
        else:
            if not n.ghost:
                g.append(f'<rect class="shadow" x="{x0 + 4:.1f}" y="{y0 + 4:.1f}" width="{n.w:.1f}" height="{n.h:.1f}"/>')
            cls = "node" + (" ghost" if n.ghost else "") + (" pill" if n.shape == "pill" else "")
            rx = n.h / 2 if n.shape == "pill" else 1.5
            g.append(f'<rect class="{cls}" x="{x0:.1f}" y="{y0:.1f}" width="{n.w:.1f}" height="{n.h:.1f}" rx="{rx:.1f}"/>')
            if n.live:
                g.append(f'<rect class="live-fill" x="{x0 + 1:.1f}" y="{y0 + 1:.1f}" width="6" height="{n.h - 2:.1f}"/>')
        ty = n.y - 8 * len(n.sub) + 5 + (7 if n.tag else 0)
        if n.tag:
            g.append(T(x0 + 9, y0 + 14, n.tag.upper(), 10, "cap", "start"))
        g.append(T(n.x, ty, n.label, 15, "lbl", mono=n.mono))
        for i, s in enumerate(n.sub):
            g.append(T(n.x, ty + 17 + 16 * i, s, 12.5, "sub"))
        if n.count:
            g.append(T(x1 - 6, y1 + 15, n.count, 11, "sub", "end", mono=True))
    if g:
        c.add("nodes", f'<g class="k-{n.kind}">' + "".join(g) + "</g>")


def _rounded(pts: list[tuple[float, float]], r: float = 10) -> str:
    d = f"M{pts[0][0]:.1f} {pts[0][1]:.1f}"
    for i in range(1, len(pts) - 1):
        (ax, ay), (bx, by), (cx, cy) = pts[i - 1], pts[i], pts[i + 1]
        l1, l2 = math.hypot(bx - ax, by - ay), math.hypot(cx - bx, cy - by)
        k = min(r, l1 / 2, l2 / 2)
        if k < 0.5:
            d += f" L{bx:.1f} {by:.1f}"
            continue
        p1 = (bx - (bx - ax) / l1 * k, by - (by - ay) / l1 * k)
        p2 = (bx + (cx - bx) / l2 * k, by + (cy - by) / l2 * k)
        d += f" L{p1[0]:.1f} {p1[1]:.1f} Q{bx:.1f} {by:.1f} {p2[0]:.1f} {p2[1]:.1f}"
    d += f" L{pts[-1][0]:.1f} {pts[-1][1]:.1f}"
    return d


def _ortho(p1, v1, p2, v2, mid=None):
    (x1, y1), (x2, y2) = p1, p2
    h1, h2 = v1[1] == 0, v2[1] == 0
    if h1 and h2:
        if abs(y1 - y2) < 0.5:
            return [p1, p2]
        xm = mid if mid is not None else (x1 + x2) / 2
        return [p1, (xm, y1), (xm, y2), p2]
    if not h1 and not h2:
        if abs(x1 - x2) < 0.5:
            return [p1, p2]
        ym = mid if mid is not None else (y1 + y2) / 2
        return [p1, (x1, ym), (x2, ym), p2]
    if h1:
        return [p1, (x2, y1), p2]
    return [p1, (x1, y2), p2]


def _along(pts, t):
    segs = [(pts[i], pts[i + 1], math.hypot(pts[i + 1][0] - pts[i][0], pts[i + 1][1] - pts[i][1])) for i in range(len(pts) - 1)]
    total = sum(s[2] for s in segs) or 1
    goal = total * t
    for a, b, length in segs:
        if goal <= length or (a, b, length) == segs[-1]:
            f = 0 if length == 0 else min(1, goal / length)
            return a[0] + (b[0] - a[0]) * f, a[1] + (b[1] - a[1]) * f
        goal -= length
    return pts[-1]


def _frame(c: Canvas, x0, y0, x1, y1, title, kind, hatch=False, dashed=False, pos="top", align="left"):
    if hatch:
        hid = c.hatch(kind)
        c.add("groups", f'<g class="k-{kind}"><rect class="hatch-band" x="{x0:.1f}" y="{y0:.1f}" width="{x1 - x0:.1f}" '
                        f'height="{y1 - y0:.1f}" fill="url(#{hid})"/><rect class="grp-in" x="{x0 + 7:.1f}" y="{y0 + 7:.1f}" '
                        f'width="{x1 - x0 - 14:.1f}" height="{y1 - y0 - 14:.1f}"/>'
                        f'<rect class="grp" x="{x0:.1f}" y="{y0:.1f}" width="{x1 - x0:.1f}" height="{y1 - y0:.1f}"/></g>')
    else:
        c.add("groups", f'<g class="k-{kind}"><rect class="grp{" dash" if dashed else ""}" x="{x0:.1f}" y="{y0:.1f}" '
                        f'width="{x1 - x0:.1f}" height="{y1 - y0:.1f}"/></g>')
    if title:
        t = title.upper()
        tw = caps_w(t, 10.5) + 16
        tx = {"left": x0, "right": x1 - tw, "center": (x0 + x1 - tw) / 2}[align]
        ty = y0 - 18 if pos == "top" else y1
        c.add("labels", f'<g class="k-{kind}"><rect class="frame-tag" x="{tx:.1f}" y="{ty:.1f}" width="{tw:.1f}" height="18"/>'
                        + T(tx + tw / 2, ty + 13, t, 10.5, "frame-tag-t") + "</g>")


def graph(name: str, title: str, desc: str, width: float, height: float, nodes: list[dict], edges: list[dict],
          groups: list[dict] | None = None, legend: list[tuple[str, str]] | None = None,
          notes: list[tuple] | None = None, raw: dict[str, list[str]] | None = None,
          tips: list[tuple[str, bool]] | None = None) -> str:
    """tips: arrowheads (kind, open) that `raw` paths reference as url(#NAME-tip-KIND[-o]).
    groups: {nodes|box, title, kind, pad, hatch, dashed, pos: top|bottom, align}.
    edges: {from, to, kind, style: solid|dash|dot, label, at, dx, dy, out, in, ox, oy, ix, iy,
            via: [(x, y)...], curve: bool, mid: elbow coordinate, both, plain, mono, width}."""
    c = Canvas(name, width, height, title, desc)
    for kind, open_ in tips or []:
        c.tip(kind, open_)
    ns = {n["id"]: Node(**n) for n in nodes}
    for g in groups or []:
        members = [ns[i] for i in g.get("nodes", [])]
        pad = g.get("pad", 22)
        if "box" in g:
            x0, y0, x1, y1 = g["box"]
        else:
            x0 = min(m.box[0] for m in members) - pad
            y0 = min(m.box[1] for m in members) - pad
            x1 = max(m.box[2] for m in members) + pad
            y1 = max(m.box[3] + (16 + 14 * len(m.sub) if m.shape == "store" else 0) for m in members) + pad
        _frame(c, x0, y0, x1, y1, g.get("title"), g.get("kind", "muted"), g.get("hatch", False), g.get("dashed", False),
               g.get("pos", "top"), g.get("align", "left"))
    for e in edges:
        a, b = ns[e["from"]], ns[e["to"]]
        kind = e.get("kind", "muted")
        style = e.get("style", "solid")
        mid = c.tip(kind, style in ("dash", "dot"))
        via = [tuple(p) for p in e.get("via", [])]
        ta = via[0] if via else (b.x, b.y)
        tb = via[-1] if via else (a.x, a.y)
        _, (x1, y1), va = _anchor(a, *ta, e.get("out"))
        _, (x2, y2), vb = _anchor(b, *tb, e.get("in"))
        if a.shape == "store" and e.get("out") is None and va[1] != 0:
            y1 += 6 * va[1]
        x1, y1 = x1 + e.get("ox", 0), y1 + e.get("oy", 0)
        x2, y2 = x2 + e.get("ix", 0), y2 + e.get("iy", 0)
        if e.get("curve"):
            k = e.get("bend", max(30, min(140, math.hypot(x2 - x1, y2 - y1) * 0.42)))
            c1 = (x1 + va[0] * k, y1 + va[1] * k)
            c2 = (x2 + vb[0] * k, y2 + vb[1] * k)
            d = f"M{x1:.1f} {y1:.1f} C{c1[0]:.1f} {c1[1]:.1f} {c2[0]:.1f} {c2[1]:.1f} {x2:.1f} {y2:.1f}"
            t = e.get("at", 0.5)
            lx = (1 - t) ** 3 * x1 + 3 * (1 - t) ** 2 * t * c1[0] + 3 * (1 - t) * t ** 2 * c2[0] + t ** 3 * x2
            ly = (1 - t) ** 3 * y1 + 3 * (1 - t) ** 2 * t * c1[1] + 3 * (1 - t) * t ** 2 * c2[1] + t ** 3 * y2
        else:
            pts = [(x1, y1)] + via + [(x2, y2)] if via else _ortho((x1, y1), va, (x2, y2), vb, e.get("mid"))
            d = _rounded(pts, e.get("r", 10))
            lx, ly = _along(pts, e.get("at", 0.5))
        cls = "edge" + {"solid": "", "dash": " dash", "dot": " dot"}[style] + (" flow" if e.get("flow") else "")
        ends = f' marker-end="url(#{mid})"' if not e.get("plain") else ""
        if e.get("both"):
            ends += f' marker-start="url(#{mid})"'
        c.add("edges", f'<g class="k-{kind}"><path class="{cls}" d="{d}" stroke-width="{e.get("width", 1.5)}"{ends}/></g>')
        if e.get("label"):
            lx += e.get("dx", 0)
            ly += e.get("dy", 0)
            ls = lines_of(e["label"])
            mono = e.get("mono", False)
            tw = max(text_w(s, 12, mono) for s in ls) + 12
            th = 15 * len(ls) + 5
            lab = [f'<rect class="lab-bg" x="{lx - tw / 2:.1f}" y="{ly - th / 2:.1f}" width="{tw:.1f}" height="{th:.1f}"/>']
            for i, s in enumerate(ls):
                lab.append(T(lx, ly - th / 2 + 14 + 15 * i, s, 12, "elabel", mono=mono))
            c.add("labels", f'<g class="k-{kind}">' + "".join(lab) + "</g>")
    for n in ns.values():
        _node_svg(c, n)
    for note in notes or []:
        nx, ny, text, kind, *rest = list(note) + ["muted"] * (4 - len(note))
        anchor = rest[0] if rest else "middle"
        c.add("labels", f'<g class="k-{kind}">' + "".join(
            T(nx, ny + 15 * i, s, 12, "elabel", anchor, halo=True) for i, s in enumerate(lines_of(text))) + "</g>")
    if legend:
        x = 20
        for label, kind in legend:
            c.add("labels", f'<g class="k-{kind}"><rect class="node" x="{x}" y="{height - 25}" width="11" height="11"/></g>')
            c.add("labels", T(x + 17, height - 15.5, label, 11.5, "sub", "start"))
            x += 34 + text_w(label, 11.5)
    for layer, items in (raw or {}).items():
        c.layers[layer] += items
    return c.svg()


# ----------------------------------------------------------------- sequence

def sequence(name: str, title: str, desc: str, participants: list[tuple], steps: list[tuple],
             width: float | None = None, gap: float = 170, step_h: float = 40) -> str:
    """participants: (id, label, sub, kind).  steps:
         ("msg", from, to, text[, style[, kind]])   style: call | reply | async | error
         ("note", ids, text[, kind])  ("frame", label, text[, ids[, kind]]) ("else", text) ("end",)
         ("gap", px)  ("cut", text)  — a crash cut across every lifeline
    """
    heads = []
    for pid, label, sub, kind in participants:
        w = max(112, text_w(label, 14.5) + 26, text_w(sub or "", 12) + 22)
        heads.append((pid, label, sub, kind, w))
    xs, cursor = {}, 24
    for pid, label, sub, kind, w in heads:
        span = max(gap, w + 14)
        xs[pid] = cursor + span / 2
        cursor += span
    tail = 0
    last = heads[-1][0]
    for st in steps:
        if st[0] == "msg" and st[1] == st[2] == last:
            tail = max(tail, 58 + max(text_w(t, 13) for t in lines_of(st[3])) - (cursor - xs[last]) + 16)
    total_w = width or cursor + 16 + max(0, tail)
    top, head_h = 18, 50
    y = top + head_h + 30
    c = Canvas(name, total_w, 10, title, desc)
    rows: list[str] = []
    frames: list[list] = []
    self_right: list[tuple[float, float]] = []
    n = 0
    for st in steps:
        kind = st[0]
        if kind == "gap":
            y += st[1]
            continue
        if kind == "cut":
            zig = " ".join(f"{x:.1f},{y + (6 if i % 2 else -6):.1f}" for i, x in enumerate(range(12, int(total_w) - 8, 14)))
            lw = text_w(st[1], 12) + 12
            rows.append(f'<g class="k-err"><polyline class="cut" points="{zig}"/>'
                        f'<rect class="lab-bg" x="12" y="{y - 26}" width="{lw:.1f}" height="18"/>'
                        + T(18, y - 12, st[1], 12, "elabel", "start") + "</g>")
            y += 34
            continue
        if kind == "frame":
            frames.append([st[1], st[2], y - 12, None, st[3] if len(st) > 3 else None, st[4] if len(st) > 4 else "muted", []])
            y += 36
            continue
        if kind == "else":
            frames[-1][6].append((y - 6, st[1]))
            y += 34
            continue
        if kind == "end":
            for f in reversed(frames):
                if f[3] is None:
                    f[3] = y - 14
                    break
            y += 10
            continue
        if kind == "note":
            ids = st[1] if isinstance(st[1], (list, tuple)) else [st[1]]
            ls = lines_of(st[2])
            x0 = min(xs[i] for i in ids) - 70
            x1 = max(xs[i] for i in ids) + 70
            widest = max(text_w(s, 12.5) for s in ls) + 24
            if x1 - x0 < widest:
                mid_x = (x0 + x1) / 2
                x0, x1 = mid_x - widest / 2, mid_x + widest / 2
            shift = max(0, 12 - x0) - max(0, x1 - (total_w - 12))
            x0, x1 = x0 + shift, x1 + shift
            h = 12 + 16 * len(ls)
            body = [f'<rect class="note" x="{x0:.1f}" y="{y - 14:.1f}" width="{x1 - x0:.1f}" height="{h:.1f}"/>']
            body += [T((x0 + x1) / 2, y + 2 + 16 * i, s, 12.5, "note-t") for i, s in enumerate(ls)]
            rows.append("<g>" + "".join(body) + "</g>")
            y += h + 12
            continue
        _, a, b, text, *rest = st
        style = rest[0] if rest else "call"
        mk = rest[1] if len(rest) > 1 else {"call": "ink", "reply": "muted", "async": "a", "error": "err"}[style]
        n += 1
        mid = c.tip(mk, style in ("reply", "async"))
        dash = {"reply": " dash", "async": " dot"}.get(style, "")
        ls = lines_of(text)
        y += 16 * (len(ls) - 1)
        tcls = "lbl" if style != "reply" else "sub"
        if a == b:
            x0 = xs[a]
            body = [f'<path class="edge{dash}" d="M{x0 + 4:.1f} {y - 8:.1f} h44 v20 h-42" marker-end="url(#{mid})"/>']
            body += [T(x0 + 56, y - 4 - 16 * (len(ls) - 1 - i), s, 13, tcls, "start", weight=500, halo=True) for i, s in enumerate(ls)]
            body.append(_badge(x0 - 4, y - 8, n))
            rows.append(f'<g class="k-{mk}">' + "".join(body) + "</g>")
            self_right.append((y, x0 + 56 + max(text_w(t, 13) for t in ls) + 14))
            y += step_h + 8
            continue
        x1, x2 = xs[a], xs[b]
        sgn = 1 if x2 > x1 else -1
        body = [f'<line class="edge{dash}" x1="{x1 + 4 * sgn:.1f}" y1="{y:.1f}" x2="{x2 - 4 * sgn:.1f}" y2="{y:.1f}" marker-end="url(#{mid})"/>']
        span = abs(x2 - x1)
        widest = max(text_w(t, 13) for t in ls)
        for i, s in enumerate(ls):
            ly = y - 9 - 16 * (len(ls) - 1 - i)
            mono = s.startswith("`")
            s2 = s.strip("`")
            if widest <= span - 56:
                body.append(T((x1 + x2) / 2 + 10 * sgn, ly, s2, 13, tcls, weight=500, mono=mono, halo=True))
            elif sgn > 0:
                body.append(T(x1 + 24, ly, s2, 13, tcls, "start", weight=500, mono=mono, halo=True))
            else:
                body.append(T(x1 - 24, ly, s2, 13, tcls, "end", weight=500, mono=mono, halo=True))
        body.append(_badge(min(x1, x2) + 12 if sgn > 0 else max(x1, x2) - 12, y, n))
        rows.append(f'<g class="k-{mk}">' + "".join(body) + "</g>")
        y += step_h
    bottom = y + 4
    c.h = bottom + head_h + 18
    for pid, label, sub, kind, w in heads:
        x = xs[pid]
        c.add("groups", f'<line class="life" x1="{x:.1f}" y1="{top + head_h:.1f}" x2="{x:.1f}" y2="{bottom:.1f}"/>')
        body = []
        for yy in (top, bottom):
            body.append(f'<rect class="shadow" x="{x - w / 2 + 4:.1f}" y="{yy + 4:.1f}" width="{w:.1f}" height="{head_h:.1f}"/>')
            body.append(f'<rect class="node" x="{x - w / 2:.1f}" y="{yy:.1f}" width="{w:.1f}" height="{head_h:.1f}" rx="1.5"/>')
            body.append(T(x, yy + (21 if sub else 30), label, 14.5, "lbl"))
            if sub:
                body.append(T(x, yy + 38, sub, 12, "sub"))
        c.add("nodes", f'<g class="k-{kind}">' + "".join(body) + "</g>")
    for label, text, y0, y1, ids, fk, elses in frames:
        if ids:
            fx0 = min(xs[i] for i in ids) - 90
            fx1 = max([max(xs[i] for i in ids) + 90] + [r for yy, r in self_right if y0 <= yy <= y1])
        else:
            fx0, fx1 = 10, total_w - 10
        t = label.upper()
        tw = caps_w(t, 10.5) + 16
        body = [f'<rect class="frame" x="{fx0:.1f}" y="{y0:.1f}" width="{fx1 - fx0:.1f}" height="{y1 - y0:.1f}"/>',
                f'<rect class="frame-tag" x="{fx0:.1f}" y="{y0:.1f}" width="{tw:.1f}" height="18"/>',
                T(fx0 + tw / 2, y0 + 13, t, 10.5, "frame-tag-t"),
                T(fx0 + tw + 8, y0 + 13.5, text, 12.5, "sub", "start", halo=True)]
        for ey, etext in elses:
            body.append(f'<line class="edge dash" x1="{fx0:.1f}" y1="{ey:.1f}" x2="{fx1:.1f}" y2="{ey:.1f}"/>')
            body.append(T(fx0 + 10, ey + 16, etext, 12.5, "sub", "start", halo=True))
        c.add("groups", f'<g class="k-{fk}">' + "".join(body) + "</g>")
    c.layers["edges"] += rows
    return c.svg()


def _badge(x, y, n):
    return (f'<rect class="badge" x="{x - 8.5:.1f}" y="{y - 8.5:.1f}" width="17" height="17"/>' + T(x, y + 4, str(n), 10.5, "badge-t"))


# ---------------------------------------------------------------- pianoroll

NOTE_NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]


def pianoroll(name: str, title: str, desc: str, notes: list[dict], width: float, height: float, beats: float,
              rows: list | None = None, lo: int | None = None, hi: int | None = None, beats_per_bar: float = 4,
              kinds: list[str] | None = None, labels: bool = True, left: float = 46, legend: list[tuple[str, str]] | None = None) -> str:
    """Notes on a pitch × time grid. notes: {at, len (beats), midi | row, vel 0..1, track}.
    rows: kit-piece names (top to bottom) for notes that carry `row` instead of `midi`; otherwise the
    grid spans MIDI lo..hi. kinds: CSS kind per track index."""
    kinds = kinds or ["accent", "a", "b", "c", "d"]
    c = Canvas(name, width, height, title, desc)
    top, bottom, right = 24, height - (30 if legend else 8), width - 8
    x_of = lambda b: left + (right - left) * b / beats
    if rows:
        n_rows = len(rows)
        y_of = lambda r: top + (bottom - top) * r / n_rows
        row_h = (bottom - top) / n_rows
        for i, rname in enumerate(rows):
            c.add("under", f'<rect class="lane{" alt" if i % 2 else ""}" x="{left:.1f}" y="{y_of(i):.1f}" '
                           f'width="{right - left:.1f}" height="{row_h:.1f}"/>')
            c.add("labels", T(left - 8, y_of(i) + row_h / 2 + 4, rname, 11, "axis", "end", mono=True))
    else:
        lo = lo if lo is not None else min(n["midi"] for n in notes) - 2
        hi = hi if hi is not None else max(n["midi"] for n in notes) + 2
        n_rows = hi - lo + 1
        row_h = (bottom - top) / n_rows
        y_of = lambda m: top + (hi - m) * row_h
        for m in range(lo, hi + 1):
            black = NOTE_NAMES[m % 12].endswith("#")
            c.add("under", f'<rect class="lane{" alt" if black else ""}" x="{left:.1f}" y="{y_of(m):.1f}" '
                           f'width="{right - left:.1f}" height="{row_h:.2f}"/>')
            if m % 12 == 0 or (row_h >= 9 and m % 12 in (4, 7)):
                c.add("labels", T(left - 8, y_of(m) + row_h / 2 + 4, f"{NOTE_NAMES[m % 12]}{m // 12 - 1}", 10.5, "axis",
                                  "end", mono=True))
    b = 0.0
    while b <= beats + 1e-9:
        bar = abs(b / beats_per_bar - round(b / beats_per_bar)) < 1e-9
        c.add("under", f'<line class="{"bar" if bar else "beat"}" x1="{x_of(b):.1f}" y1="{top:.1f}" x2="{x_of(b):.1f}" '
                       f'y2="{bottom:.1f}"/>')
        if bar and b < beats - 1e-9:
            c.add("labels", T(x_of(b) + 4, top - 8, str(int(round(b / beats_per_bar)) + 1), 10.5, "axis", "start", mono=True))
        b += 1
    for n in notes:
        x0, x1 = x_of(n["at"]), x_of(n["at"] + n["len"])
        y = y_of(n["row"]) if rows else y_of(n["midi"])
        h = row_h
        kind = kinds[n.get("track", 0) % len(kinds)]
        op = 0.35 + 0.65 * max(0.0, min(1.0, n.get("vel", 0.8)))
        inset = 1 if h > 5 else 0.3
        c.add("nodes", f'<g class="k-{kind}"><rect class="note" x="{x0 + 0.5:.1f}" y="{y + inset:.1f}" '
                       f'width="{max(2.0, x1 - x0 - 1):.1f}" height="{max(1.5, h - 2 * inset):.1f}" rx="1" '
                       f'fill-opacity="{op:.2f}"/></g>')
        if labels and n.get("label") and x1 - x0 > text_w(n["label"], 10, True) + 6:
            c.add("labels", T(x0 + 4, y + h / 2 + 3.5, n["label"], 10, "note-l", "start", mono=True))
    if legend:
        x = left
        for label, kind in legend:
            c.add("labels", f'<g class="k-{kind}"><rect class="note" x="{x}" y="{height - 19}" width="11" height="11" rx="1"/></g>')
            c.add("labels", T(x + 17, height - 9.5, label, 11.5, "sub", "start"))
            x += 34 + text_w(label, 11.5)
    return c.svg()


# ----------------------------------------------------------------- waveform

def waveform(name: str, title: str, desc: str, peaks: list[tuple[float, float]], width: float, height: float,
             seconds: float, kind: str = "accent", marks: list[tuple[float, str]] | None = None, axis: bool = True) -> str:
    """A mirrored min/max envelope of real audio: peaks[i] = (min, max) of column i, in -1..1."""
    c = Canvas(name, width, height, title, desc)
    bottom_pad = 18 if axis else 2
    mid, amp = (height - bottom_pad) / 2, (height - bottom_pad) / 2 - 2
    n = len(peaks)
    xs = [width * i / max(1, n - 1) for i in range(n)]
    upper = " ".join(f"{x:.1f},{mid - mx * amp:.1f}" for x, (_, mx) in zip(xs, peaks))
    lower = " ".join(f"{x:.1f},{mid - mn * amp:.1f}" for x, (mn, _) in reversed(list(zip(xs, peaks))))
    c.add("under", f'<line class="zero" x1="0" y1="{mid:.1f}" x2="{width:.1f}" y2="{mid:.1f}"/>')
    c.add("nodes", f'<g class="k-{kind}"><polygon class="wave" points="{upper} {lower}"/></g>')
    for t, label in marks or []:
        x = width * t / seconds
        c.add("labels", f'<line class="beat" x1="{x:.1f}" y1="0" x2="{x:.1f}" y2="{height - bottom_pad:.1f}"/>'
                        + T(x + 4, 11, label, 10.5, "axis", "start", mono=True))
    if axis:
        step = 0.5 if seconds <= 4 else 1 if seconds <= 10 else 2 if seconds <= 24 else 5
        t = 0.0
        while t <= seconds + 1e-9:
            x = min(width - 1, width * t / seconds)
            c.add("labels", f'<line class="beat" x1="{x:.1f}" y1="{height - bottom_pad:.1f}" x2="{x:.1f}" y2="{height - bottom_pad + 4:.1f}"/>')
            if t < seconds - 1e-9:
                c.add("labels", T(x + 3, height - 3, f"{t:g}s", 10, "axis", "start", mono=True))
            t += step
    return c.svg()


# ---------------------------------------------------------------- xml anatomy

def xml_anatomy(name: str, title: str, desc: str, xml: str, width: float = 900, kinds: list[str] | None = None,
                skip_attrs: tuple[str, ...] = ("version", "xmlns", "xmlns:mstts")) -> str:
    """Nested boxes for a real XML document: one box per element, its tag and attributes on the
    first line, its text inside the innermost box."""
    import xml.etree.ElementTree as ET
    kinds = kinds or ["ink", "a", "b", "c", "accent", "d"]
    ns = {"http://www.w3.org/2001/10/synthesis": "", "http://www.w3.org/2001/mstts": "mstts:",
          "http://www.w3.org/XML/1998/namespace": "xml:"}

    def qn(tag: str) -> str:
        if tag.startswith("{"):
            uri, local = tag[1:].split("}", 1)
            return ns.get(uri, "") + local
        return tag

    root = ET.fromstring(xml)
    chain, el = [], root
    while True:
        chain.append(el)
        kids = list(el)
        if not kids:
            break
        el = kids[0]
    line_h, pad = 26, 16
    depth = len(chain)
    height = 24 + depth * (line_h + pad) + 56 + depth * pad
    c = Canvas(name, width, height, title, desc)
    x0, y0, x1 = 10, 12, width - 10
    y1 = height - 12
    for i, el in enumerate(chain):
        kind = kinds[i % len(kinds)]
        bx0, by0, bx1, by1 = x0 + i * pad, y0 + i * (line_h + pad), x1 - i * pad, y1 - i * pad
        attrs = [(qn(k), v) for k, v in el.attrib.items() if qn(k) not in skip_attrs]
        c.add("groups", f'<g class="k-{kind}"><rect class="xbox" x="{bx0:.1f}" y="{by0:.1f}" width="{bx1 - bx0:.1f}" '
                        f'height="{by1 - by0:.1f}" rx="4"/></g>')
        label = "<" + qn(el.tag)
        tx = bx0 + 12
        c.add("labels", f'<g class="k-{kind}">' + T(tx, by0 + 19, label, 13.5, "xtag", "start", mono=True) + "</g>")
        tx += text_w(label, 13.5, True) + 8
        for k, v in attrs:
            part = f'{k}="{v}"'
            c.add("labels", f'<g class="k-{kind}">' + T(tx, by0 + 19, k, 12.5, "xattr", "start", mono=True)
                  + T(tx + text_w(k + "=", 12.5, True), by0 + 19, f'"{v}"', 12.5, "xval", "start", mono=True) + "</g>")
            tx += text_w(part, 12.5, True) + 10
        c.add("labels", f'<g class="k-{kind}">' + T(tx - 6, by0 + 19, ">", 13.5, "xtag", "start", mono=True) + "</g>")
        if el.text and el.text.strip() and not list(el):
            c.add("labels", T((bx0 + bx1) / 2, (by0 + line_h + by1) / 2 + 6, el.text.strip(), 18, "xtext", "middle"))
    return c.svg()
