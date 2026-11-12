#!/usr/bin/env python3
"""Draws the Rays app icon: `python3 packaging/icon.py` writes packaging/Rays.icns (macOS: sips, iconutil).

The Rays theme's paper, ink and accent; a fan of rays between two parentheses. Standard library only.
"""
import math, os, struct, subprocess, sys, tempfile, zlib

N = 1024
PAPER, PAPER_LOW, INK, ACCENT = (0xF4, 0xF5, 0xF0), (0xE4, 0xE8, 0xE2), (0x22, 0x2B, 0x2B), (0xF0, 0x48, 0x1F)
TILE, RADIUS = 412.0, 186.0  # half the 824-point tile of the macOS icon grid, and its corner radius
MID = N / 2
GLYPH = 1.2


def tile(x, y, dy=0.0):
    qx, qy = abs(x - MID) - TILE + RADIUS, abs(y - MID - dy) - TILE + RADIUS
    return math.hypot(max(qx, 0.0), max(qy, 0.0)) + min(max(qx, qy), 0.0) - RADIUS


def segment(x, y, a, b, half):
    (ax, ay), (bx, by) = a, b
    dx, dy = bx - ax, by - ay
    t = max(0.0, min(1.0, ((x - ax) * dx + (y - ay) * dy) / (dx * dx + dy * dy)))
    return math.hypot(x - ax - t * dx, y - ay - t * dy) - half


def paren(x, y, cx, r, sweep, half, left):
    """An arc of the circle at (cx, MID), `sweep` radians either side of the horizontal, round ends."""
    px, py = (cx - x if left else x - cx), y - MID
    if abs(math.atan2(py, px)) <= sweep:
        return abs(math.hypot(px, py) - r) - half
    ex, ey = r * math.cos(sweep), r * math.sin(sweep)
    return math.hypot(px - ex, abs(py) - ey) - half


ORIGIN, FAR, REACH = (336.0, MID), 434.0, 246.0  # rays leave ORIGIN and stop short of the right parenthesis
RAYS = []
for step in range(-3, 4):
    angle = math.radians(step * 13.5)
    c, s = math.cos(angle), math.sin(angle)
    off = ORIGIN[0] - FAR
    end = -off * c + math.sqrt(REACH * REACH - off * off * s * s)
    RAYS.append(((ORIGIN[0] + 104 * c, MID + 104 * s), (ORIGIN[0] + end * c, MID + end * s), 13.0 if step == 0 else 9.0, step == 0))


def over(dst, colour, alpha):
    return tuple(d + (c - d) * alpha for d, c in zip(dst, colour))


def cover(d):
    return max(0.0, min(1.0, 0.5 - d))


def pixel(x, y):
    shadow = 0.30 * max(0.0, min(1.0, 0.5 - tile(x, y, 12.0) / 28.0))  # a soft drop below the tile
    a = cover(tile(x, y))
    if a <= 0.0:
        return (0, 0, 0, shadow)
    t = max(0.0, min(1.0, (y - (MID - TILE)) / (2 * TILE)))
    rgb = tuple(p + (q - p) * t for p, q in zip(PAPER, PAPER_LOW))
    x, y = MID + (x - MID) / GLYPH, MID + (y - MID) / GLYPH  # the drawing, enlarged about the centre
    if 250 < x < 780 and 280 < y < 744:
        rgb = over(rgb, INK, cover(GLYPH * paren(x, y, 590.0, 300.0, math.radians(42), 23.0, True)))
        rgb = over(rgb, INK, cover(GLYPH * paren(x, y, FAR, 300.0, math.radians(42), 23.0, False)))
        for p, q, half, hot in RAYS:
            rgb = over(rgb, ACCENT if hot else INK, cover(GLYPH * segment(x, y, p, q, half)))
        rgb = over(rgb, ACCENT, cover(GLYPH * (math.hypot(x - ORIGIN[0], y - ORIGIN[1]) - 34.0)))
    alpha = a + shadow * (1.0 - a)
    return tuple(c * a / alpha for c in rgb) + (alpha,)


def png(path):
    rows = bytearray()
    for j in range(N):
        rows.append(0)
        for i in range(N):
            r, g, b, a = pixel(i + 0.5, j + 0.5)
            rows += bytes((round(r), round(g), round(b), round(a * 255)))
    def chunk(kind, data):
        return struct.pack(">I", len(data)) + kind + data + struct.pack(">I", zlib.crc32(kind + data))
    with open(path, "wb") as f:
        f.write(b"\x89PNG\r\n\x1a\n" + chunk(b"IHDR", struct.pack(">IIBBBBB", N, N, 8, 6, 0, 0, 0))
                + chunk(b"IDAT", zlib.compress(bytes(rows), 9)) + chunk(b"IEND", b""))


if __name__ == "__main__":
    here = os.path.dirname(os.path.abspath(__file__))
    with tempfile.TemporaryDirectory() as tmp:
        master, iconset = os.path.join(tmp, "icon.png"), os.path.join(tmp, "Rays.iconset")
        png(master)
        if len(sys.argv) > 1:  # `icon.py out.png`: the 1024-pixel master only
            os.replace(master, sys.argv[1])
            sys.exit(0)
        os.mkdir(iconset)
        for size in (16, 32, 128, 256, 512):
            for scale in (1, 2):
                name = f"icon_{size}x{size}{'@2x' if scale == 2 else ''}.png"
                subprocess.run(["sips", "-z", str(size * scale), str(size * scale), master, "--out", os.path.join(iconset, name)],
                               check=True, stdout=subprocess.DEVNULL)
        subprocess.run(["iconutil", "-c", "icns", iconset, "-o", os.path.join(here, "Rays.icns")], check=True)
