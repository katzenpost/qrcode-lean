#!/usr/bin/env python3
"""Writes the decoder's test vectors, tests/vectors/decode/, with quirc 1.2 as the
reference decoder and qrcodegen 1.8.0 drawing the symbols:

    pip install qrcodegen==1.8.0
    git clone --depth 1 --branch v1.2 https://github.com/dlbeer/quirc
    python3 tools/gen-decode-vectors.py quirc tests/vectors

It builds tools/quirc-harness.c against the quirc checkout with the system C compiler,
renders the test images, and records what quirc makes of each vector: Reed-Solomon
blocks with errors, format bits with errors, module grids with errors, and the images
(plus the camera photos in tests/photos/, which are inputs, not generated).

Images are rendered with integer arithmetic and IEEE basic operations only, and noise
comes from a seeded generator, so the output is the same on every machine.
"""

import json, os, random, subprocess, sys, tempfile
from fractions import Fraction

import qrcodegen
from qrcodegen import QrCode, QrSegment

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PHOTOS = os.path.join(ROOT, "tests", "photos")
QUIRC_COMMIT = "542848dd6b9b0eaa9587bbf25b9bc67bd8a71fca"  # v1.2
ECCS = [QrCode.Ecc.LOW, QrCode.Ecc.MEDIUM, QrCode.Ecc.QUARTILE, QrCode.Ecc.HIGH]
VOUCHER = "cvV8Q1uGC31DYm4pNGXfCg8ihJA1Pj7sYkyK/eBIP28="
rng = random.Random(0xDEC0DE)


class Quirc:
    def __init__(self, quirc_dir, tmp):
        lib = os.path.join(quirc_dir, "lib")
        exe = os.path.join(tmp, "quirc-harness")
        srcs = [os.path.join(lib, f) for f in ("quirc.c", "identify.c", "version_db.c")]
        subprocess.run(["cc", "-O2", "-I", lib, "-o", exe, os.path.join(ROOT, "tools", "quirc-harness.c")]
                       + srcs + ["-lm"], check=True)
        self.p = subprocess.Popen([exe], stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)

    def ask(self, req):
        self.p.stdin.write(req + "\n")
        self.p.stdin.flush()
        return self.p.stdout.readline().split()

    def decoded(self, words):
        if words[0] == "err":
            return {"error": " ".join(words[1:])}
        return {"version": int(words[1]), "ecc": words[2], "mask": int(words[3]),
                "payload": words[4] if len(words) > 4 else ""}

    def image(self, path, w, h):
        self.p.stdin.write(f"image {w} {h} {path}\n")
        self.p.stdin.flush()
        codes = []
        for _ in range(int(self.p.stdout.readline().split()[1])):
            c = self.p.stdout.readline().split()
            corners = [int(v) for v in c[2:10]]
            codes.append({"size": int(c[1]), "corners": [corners[i:i + 2] for i in range(0, 8, 2)],
                          "modules": c[10], **self.decoded(self.p.stdout.readline().split())})
        return codes


def pack(grid):
    flat = [c for row in grid for c in row]
    out = bytearray((len(flat) + 7) // 8)
    for i, c in enumerate(flat):
        if c:
            out[i >> 3] |= 0x80 >> (i & 7)
    return out.hex()


def modules(q):
    return [[q.get_module(x, y) for x in range(q.get_size())] for y in range(q.get_size())]


def reed_solomon(qz):
    """Blocks for every parity length QR Codes use, with 0 to t + 2 bytes wrong."""
    out = []
    for npar in sorted({e for row in QrCode._ECC_CODEWORDS_PER_BLOCK for e in row if e > 0}):
        div = QrCode._reed_solomon_compute_divisor(npar)
        for dlen in (1, 19, 60, 123):
            data = bytes(rng.randrange(256) for _ in range(dlen))
            block = bytearray(data + QrCode._reed_solomon_compute_remainder(data, div))
            for k in range(npar // 2 + 3):
                bad = bytearray(block)
                for p in rng.sample(range(len(bad)), k):
                    bad[p] ^= rng.randrange(1, 256)
                r = qz.ask(f"rs {npar} {bad.hex()}")
                rec = {"parity": npar, "errors": k, "block": bad.hex()}
                if r[0] == "ok":
                    rec["corrected"] = r[1]
                out.append(rec)
    return out


def format_bits(qz):
    """Every level and mask's format bits as drawn by qrcodegen, with 0 to 5 bits wrong."""
    out = []
    for e in ECCS:
        for m in range(8):
            q = object.__new__(QrCode)
            q._version, q._size, q._errcorlvl = 1, 21, e
            q._modules = [[False] * 21 for _ in range(21)]
            q._isfunction = [[False] * 21 for _ in range(21)]
            q._draw_format_bits(m)
            bits = [q._modules[i][8] for i in range(6)] + [q._modules[7][8], q._modules[8][8], q._modules[8][7]] + \
                [q._modules[8][14 - i] for i in range(9, 15)]
            word = sum(int(x) << i for i, x in enumerate(bits))
            for k in (0, 1, 2, 3, 4, 5):
                bad = word
                for p in rng.sample(range(15), k):
                    bad ^= 1 << p
                r = qz.ask(f"fmt {bad}")
                rec = {"bits": bad, "errors": k}
                if r[0] == "ok":
                    rec["ecc"], rec["mask"] = r[1], int(r[2])
                out.append(rec)
    return out


def grids(qz):
    """Whole symbols from qrcodegen, every segment mode, with modules flipped at random
    anywhere in the symbol, as a sampler's misreads would."""
    sj = "漢字モード".encode("shift_jis")
    kanji = qrcodegen._BitBuffer()
    for i in range(0, len(sj), 2):
        c = sj[i] << 8 | sj[i + 1]
        c -= 0x8140 if c <= 0x9FFC else 0xC140
        kanji.append_bits((c >> 8) * 0xC0 + (c & 0xFF), 13)
    cases = [
        ([QrSegment.make_bytes(VOUCHER.encode())], 1, -1),
        ([QrSegment.make_numeric("31415926535897932384626433832795")], 0, 3),
        ([QrSegment.make_alphanumeric("HTTPS://KATZENPOST.NETWORK/ $%*+-./:")], 2, 5),
        ([QrSegment(QrSegment.Mode.KANJI, len(sj) // 2, kanji)], 3, 1),
        ([QrSegment.make_eci(26), QrSegment.make_bytes("Grüße".encode()), QrSegment.make_numeric("0123")], 1, 7),
        ([QrSegment.make_bytes(bytes(rng.randrange(256) for _ in range(300)))], 0, 4),
        ([QrSegment.make_bytes(bytes(rng.randrange(256) for _ in range(1000)))], 2, 6),
        ([QrSegment.make_bytes(bytes(rng.randrange(256) for _ in range(2900)))], 0, 0),
    ]
    out = []
    for segs, e, mask in cases:
        q = QrCode.encode_segments(segs, ECCS[e], 1, 40, mask, False)
        n = q.get_size()
        for flips in (0, 3, n // 2, n * 2, n * n // 25):
            g = modules(q)
            for p in rng.sample(range(n * n), flips):
                g[p // n][p % n] ^= True
            rec = {"size": n, "flips": flips, "modules": pack(g)}
            out.append({**rec, **qz.decoded(qz.ask(f"grid {n} {rec['modules']}"))})
    return out


# -- images --------------------------------------------------------------------

def homography(src, dst):
    """The exact projective map taking the four points src to dst, as a 3x3 matrix."""
    rows, rhs = [], []
    for (x, y), (u, v) in zip(src, dst):
        x, y, u, v = map(Fraction, (x, y, u, v))
        rows.append([x, y, 1, 0, 0, 0, -u * x, -u * y]); rhs.append(u)
        rows.append([0, 0, 0, x, y, 1, -v * x, -v * y]); rhs.append(v)
    n = 8
    a = [r + [b] for r, b in zip(rows, rhs)]
    for c in range(n):
        p = next(r for r in range(c, n) if a[r][c] != 0)
        a[c], a[p] = a[p], a[c]
        for r in range(n):
            if r != c and a[r][c] != 0:
                f = a[r][c] / a[c][c]
                a[r] = [x - f * y for x, y in zip(a[r], a[c])]
    h = [a[i][n] / a[i][i] for i in range(n)] + [Fraction(1)]
    return [h[0:3], h[3:6], h[6:9]]


def apply(h, x, y):
    w = h[2][0] * x + h[2][1] * y + h[2][2]
    return ((h[0][0] * x + h[0][1] * y + h[0][2]) / w, (h[1][0] * x + h[1][1] * y + h[1][2]) / w)


def adjugate(m):
    (a, b, c), (d, e, f), (g, h, i) = m
    return [[e * i - f * h, c * h - b * i, b * f - c * e],
            [f * g - d * i, a * i - c * g, c * d - a * f],
            [d * h - e * g, b * g - a * h, a * e - b * d]]


def render(grid, w, h, corners, light=225, dark=35, bg=None, quiet=4, ss=4):
    """Pixel values for `grid` drawn with its corners at `corners` (top left, top right,
    bottom right, bottom left of the module area), averaged over ss x ss points per
    pixel. Within `quiet` modules of the symbol is light, beyond is `bg`."""
    n = len(grid)
    fwd = homography([(0, 0), (n, 0), (n, n), (0, n)], corners)
    inv = [[float(v) for v in row] for row in adjugate(fwd)]
    bg = light if bg is None else bg
    img = bytearray(w * h)
    for py in range(h):
        for px in range(w):
            acc = 0
            for j in range(ss):
                for i in range(ss):
                    u, v = apply(inv, px + (i + 0.5) / ss, py + (j + 0.5) / ss)
                    if -quiet <= u < n + quiet and -quiet <= v < n + quiet:
                        acc += dark if 0 <= u < n and 0 <= v < n and grid[int(v)][int(u)] else light
                    else:
                        acc += bg
            img[py * w + px] = acc // (ss * ss)
    return img, [tuple(float(c) for c in apply(fwd, x, y)) for x, y in [(3.5, 3.5), (n - 3.5, 3.5), (3.5, n - 3.5)]]


def similar(n, a, b, scale_margin):
    """Corners of a symbol turned and scaled by the integer vector (a, b) per side, and an
    image size holding it and its quiet zone with a margin."""
    qa, qb = Fraction(a * 4, n), Fraction(b * 4, n)  # four modules
    pts = [(0, 0), (a, b), (a - b, b + a), (-b, a)]
    # the quiet zone's outer corners: each corner pushed out along both sides
    outer = [(pts[0][0] - qa + qb, pts[0][1] - qb - qa), (pts[1][0] + qa + qb, pts[1][1] + qb - qa),
             (pts[2][0] + qa - qb, pts[2][1] + qb + qa), (pts[3][0] - qa - qb, pts[3][1] - qb + qa)]
    xs, ys = [p[0] for p in outer], [p[1] for p in outer]
    ox, oy = scale_margin - min(xs), scale_margin - min(ys)
    w, h = int(max(xs) - min(xs)) + 2 * scale_margin, int(max(ys) - min(ys)) + 2 * scale_margin
    return [(x + ox, y + oy) for x, y in pts], w, h


def gradient(img, w, h, gx, gy):
    """Uneven light: scales each pixel by 1 + (gx x / w + gy y / h) / 100."""
    return bytearray(min(255, img[y * w + x] * (100 + gx * x // w + gy * y // h) // 100)
                     for y in range(h) for x in range(w))


def blur(img, w, h):
    """3x3 box blur, edges clamped."""
    out = bytearray(w * h)
    for y in range(h):
        for x in range(w):
            s = sum(img[min(h - 1, max(0, y + j)) * w + min(w - 1, max(0, x + i))] for j in (-1, 0, 1) for i in (-1, 0, 1))
            out[y * w + x] = s // 9
    return out


def noise(img, amp):
    return bytearray(min(255, max(0, v + rng.randrange(-amp, amp + 1) + rng.randrange(-amp, amp + 1))) for v in img)


def mirror(img, w, h):
    return bytearray(img[y * w + (w - 1 - x)] for y in range(h) for x in range(w))


def write_pgm(path, w, h, img):
    with open(path, "wb") as f:
        f.write(b"P5\n%d %d\n255\n" % (w, h) + bytes(img))


def read_pgm(path):
    with open(path, "rb") as f:
        data = f.read()
    fields, pos = [], 0
    while len(fields) < 4:
        while data[pos:pos + 1].isspace():
            pos += 1
        if data[pos:pos + 1] == b"#":
            pos = data.index(b"\n", pos)
            continue
        end = pos
        while not data[end:end + 1].isspace():
            end += 1
        fields.append(data[pos:end])
        pos = end
    assert fields[0] == b"P5" and fields[3] == b"255", path
    w, h = int(fields[1]), int(fields[2])
    return w, h, data[pos + 1:pos + 1 + w * h]


def images():
    """(name, text, width, height, pixels, finder centres) for each test image."""
    voucher = QrCode.encode_segments([QrSegment.make_bytes(VOUCHER.encode())], QrCode.Ecc.MEDIUM, 1, 40, -1, False)
    g = modules(voucher)
    n = len(g)
    out = []

    def add(name, grid, text, w, h, corners, post=lambda img, w, h: img, mirrored=False, **kw):
        img, finders = render(grid, w, h, corners, **kw)
        if mirrored:
            img, finders = mirror(img, w, h), [(w - x, y) for x, y in finders]
        out.append((name, text, w, h, post(img, w, h), finders))

    # 4 pixels a module, square to the image
    add("clean", g, VOUCHER, 164, 164, [(16, 16), (148, 16), (148, 148), (16, 148)])
    # 2.5 pixels a module, off the pixel grid, on a dark surround
    add("small", g, VOUCHER, 112, 108, [(Fraction(29, 2), 13), (97, 13), (97, Fraction(191, 2)), (Fraction(29, 2), Fraction(191, 2))], bg=70)
    # turned about 30 degrees, 3 pixels a module
    c, w, h = similar(n, 86, 50, 6)
    add("rotated", g, VOUCHER, w, h, c, bg=90)
    # turned about 200 degrees: the top left finder ends up bottom right
    c, w, h = similar(n, -94, -34, 6)
    add("upside-down", g, VOUCHER, w, h, c, bg=90)
    # seen from below and to the left
    add("perspective", g, VOUCHER, 180, 160, [(42, 30), (150, 46), (138, 128), (34, 140)], bg=60)
    # a phone screen in a dim room: blur, noise, uneven light, low contrast
    c, w, h = similar(n, 100, 12, 8)
    add("noisy", g, VOUCHER, w, h, c, light=180, dark=70, bg=40,
        post=lambda img, w, h: noise(blur(gradient(img, w, h, 50, -30), w, h), 14))
    # mirrored, as some front cameras deliver frames
    c, w, h = similar(n, 99, -14, 6)
    add("mirrored", g, VOUCHER, w, h, c, mirrored=True)
    # version 8, so with version information, slightly skewed
    text = "https://katzenpost.network/ is a mix network; this symbol carries version information, " \
           "and the decoder reads it to size the grid."
    big = QrCode.encode_segments([QrSegment.make_bytes(text.encode())], QrCode.Ecc.MEDIUM, 1, 40, -1, False)
    assert big.get_version() == 8
    add("version8", modules(big), text, 176, 172, [(18, 16), (160, 20), (158, 156), (14, 152)], bg=80)
    return out


def main(quirc_dir, outdir):
    head = subprocess.run(["git", "-C", quirc_dir, "rev-parse", "HEAD"], capture_output=True, text=True).stdout.strip()
    assert head == QUIRC_COMMIT, f"want quirc {QUIRC_COMMIT}, have {head}"
    out = os.path.join(outdir, "decode")
    os.makedirs(out, exist_ok=True)
    with tempfile.TemporaryDirectory() as tmp:
        qz = Quirc(quirc_dir, tmp)
        vec = {"reed_solomon": reed_solomon(qz), "format": format_bits(qz), "grids": grids(qz), "images": []}
        shots = [(name, text, w, h, img, finders, "decode/" + name + ".pgm")
                 for name, text, w, h, img, finders in images()]
        for f in sorted(os.listdir(PHOTOS)) if os.path.isdir(PHOTOS) else []:
            if f.endswith(".pgm"):
                w, h, img = read_pgm(os.path.join(PHOTOS, f))
                shots.append((f[:-4], VOUCHER, w, h, img, None, "../photos/" + f))
        for name, text, w, h, img, finders, rel in shots:
            if finders is not None:
                write_pgm(os.path.join(out, name + ".pgm"), w, h, img)
            raw = os.path.join(tmp, "image.raw")
            with open(raw, "wb") as f:
                f.write(img)
            rec = {"name": name, "file": rel, "width": w, "height": h, "text": text, "quirc": qz.image(raw, w, h)}
            if finders is not None:
                rec["finders"] = [[round(x, 3), round(y, 3)] for x, y in finders]
            vec["images"].append(rec)
    with open(os.path.join(out, "decode.json"), "w") as f:
        json.dump(vec, f, indent=1)
        f.write("\n")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
