#!/usr/bin/env python3
"""Writes the test vectors under tests/vectors/ from Project Nayuki's qrcodegen.

Run with the Python package `qrcodegen` 1.8.0 (pip install qrcodegen==1.8.0):

    python3 tools/gen-vectors.py tests/vectors

It calls the library's private steps directly, so each component of the Lean port
can be checked on its own. Inputs come from a fixed seed; the output is stable.
"""

import json, os, random, sys

import qrcodegen
from qrcodegen import QrCode, QrSegment

assert qrcodegen.__file__ and QrCode.MAX_VERSION == 40

ECCS = [QrCode.Ecc.LOW, QrCode.Ecc.MEDIUM, QrCode.Ecc.QUARTILE, QrCode.Ecc.HIGH]
ECC_NAME = "LMQH"
rng = random.Random(0x5172)


def bits(bb):
    return "".join("1" if b else "0" for b in bb)


def pack(grid):
    """Rows of booleans, row-major, most significant bit first, as hex."""
    flat = [c for row in grid for c in row]
    out = bytearray((len(flat) + 7) // 8)
    for i, c in enumerate(flat):
        if c:
            out[i >> 3] |= 0x80 >> (i & 7)
    return out.hex()


def fnv1a(data):
    h = 0xCBF29CE484222325
    for b in data:
        h = ((h ^ b) * 0x100000001B3) & 0xFFFFFFFFFFFFFFFF
    return f"{h:016x}"


def write(outdir, component, obj):
    os.makedirs(os.path.join(outdir, component), exist_ok=True)
    with open(os.path.join(outdir, component, component + ".json"), "w") as f:
        json.dump(obj, f, indent=1)
        f.write("\n")


def bare(version, ecl):
    """A QrCode with fields set but nothing drawn."""
    q = object.__new__(QrCode)
    q._version, q._size, q._errcorlvl = version, version * 4 + 17, ecl
    q._modules = [[False] * q._size for _ in range(q._size)]
    q._isfunction = [[False] * q._size for _ in range(q._size)]
    return q


def payload(n):
    return bytes(rng.randrange(256) for _ in range(n))


def gf256_and_rs():
    mul = QrCode._reed_solomon_multiply
    pairs = [(x, y) for x in (0, 1, 2, 0x80, 0xFF) for y in (0, 1, 2, 0x8E, 0xFF)]
    pairs += [(rng.randrange(256), rng.randrange(256)) for _ in range(500)]
    divisors = {d: QrCode._reed_solomon_compute_divisor(d).hex() for d in range(1, 31)}
    rems = []
    for d in sorted({e for row in QrCode._ECC_CODEWORDS_PER_BLOCK for e in row if e > 0}):
        for n in (0, 1, 19, 123):
            data = payload(n)
            rem = QrCode._reed_solomon_compute_remainder(data, QrCode._reed_solomon_compute_divisor(d))
            rems.append({"degree": d, "data": data.hex(), "remainder": rem.hex()})
    return {"mul": [[x, y, mul(x, y)] for x, y in pairs], "divisors": divisors, "remainders": rems}


def kanji_segment(text):
    """Kanji mode from Shift JIS, ISO/IEC 18004 8.4.5; the Python library has no kanji mode."""
    sjis = text.encode("shift_jis")
    bb = qrcodegen._BitBuffer()
    for i in range(0, len(sjis), 2):
        c = sjis[i] << 8 | sjis[i + 1]
        c -= 0x8140 if c <= 0x9FFC else 0xC140
        bb.append_bits((c >> 8) * 0xC0 + (c & 0xFF), 13)
    return sjis, QrSegment(QrSegment.Mode.KANJI, len(sjis) // 2, bb)


MODE = {QrSegment.Mode.NUMERIC: "numeric", QrSegment.Mode.ALPHANUMERIC: "alphanumeric",
        QrSegment.Mode.BYTE: "byte", QrSegment.Mode.KANJI: "kanji", QrSegment.Mode.ECI: "eci"}


def seg_json(s):
    return {"mode": MODE[s.get_mode()], "num_chars": s.get_num_chars(), "bits": bits(s.get_data())}


TEXTS = ["", "0", "01234567", "0123456789012345", "A", "HELLO WORLD", "AC-42", "$%*+-./: ",
         "hello", "Hello, world!", "こんにちwa、世界！ αβγδ", "cvV8Q1uGC31DYm4pNGXfCg8ihJA1Pj7sYkyK/eBIP28=",
         "314159265358979323846264338327950288419716939937510", "HTTPS://EXAMPLE.ORG/A/B"]


def segments():
    out = {"numeric": [], "alphanumeric": [], "bytes": [], "eci": [], "kanji": [], "make_segments": [],
           "total_bits": []}
    for t in TEXTS:
        if QrSegment.is_numeric(t):
            out["numeric"].append({"text": t, **seg_json(QrSegment.make_numeric(t))})
        if QrSegment.is_alphanumeric(t):
            out["alphanumeric"].append({"text": t, **seg_json(QrSegment.make_alphanumeric(t))})
        out["make_segments"].append({"text": t, "segments": [seg_json(s) for s in QrSegment.make_segments(t)]})
    for t in ["0", "!", "a", "HELLO world"]:
        out["make_segments"].append({"text": t, "segments": [seg_json(s) for s in QrSegment.make_segments(t)]})
    for n in (0, 1, 5, 44, 300):
        d = payload(n)
        out["bytes"].append({"data": d.hex(), **seg_json(QrSegment.make_bytes(d))})
    for v in (0, 1, 26, 127, 128, 999, 16383, 16384, 999999):
        out["eci"].append({"value": v, **seg_json(QrSegment.make_eci(v))})
    for t in ["点", "茗荷", "漢字モード"]:
        sjis, s = kanji_segment(t)
        out["kanji"].append({"sjis": sjis.hex(), **seg_json(s)})
    # outside kanji mode: a user-area code, a lone byte, a bad second byte
    for sjis in ["f040", "81", "817f", "ebc0"]:
        out["kanji"].append({"sjis": sjis, "segment": None})
    # character counts at the field width boundaries: 255 bytes fit 8 bits, 256 do not
    for n in (255, 256):
        segs = [QrSegment.make_bytes(bytes(n))]
        out["total_bits"].append({"byte_len": n, "bits": [QrSegment.get_total_bits(segs, v) for v in range(1, 41)]})
    return out


def formula_data(v, e):
    """Data codewords for version v and level e, by a formula the tests repeat."""
    return bytes((i * 151 + v * 7 + e.ordinal * 53) & 0xFF for i in range(QrCode._get_num_data_codewords(v, e)))


def capacity_and_codewords():
    caps = []
    for v in range(1, 41):
        caps.append({"version": v, "raw_modules": QrCode._get_num_raw_data_modules(v),
                     "alignment": bare(v, ECCS[0])._get_alignment_pattern_positions(),
                     "data_codewords": [QrCode._get_num_data_codewords(v, e) for e in ECCS]})
    inter = []
    for v in range(1, 41):
        for e in ECCS:
            q = bare(v, e)
            data = formula_data(v, e)
            out = bytes(q._add_ecc_and_interleave(bytearray(data)))
            rec = {"version": v, "ecc": ECC_NAME[e.ordinal], "fnv1a": fnv1a(out)}
            if v <= 7 or v in (13, 40):
                rec["codewords"] = out.hex()
            inter.append(rec)
    return {"capacity": caps}, {"interleave": inter}


def function_patterns():
    fmt = []
    for e in ECCS:
        for m in range(8):
            q = bare(1, e)
            q._draw_format_bits(m)
            # recover the 15 bits from their first copy
            b = [q._modules[i][8] for i in range(6)] + [q._modules[7][8], q._modules[8][8], q._modules[8][7]] + \
                [q._modules[8][14 - i] for i in range(9, 15)]
            fmt.append({"ecc": ECC_NAME[e.ordinal], "mask": m, "bits": sum(int(x) << i for i, x in enumerate(b))})
    vers = []
    for v in range(7, 41):
        q = bare(v, ECCS[0])
        q._draw_version()
        b = [q._modules[i // 3][q._size - 11 + i % 3] for i in range(18)]
        vers.append({"version": v, "bits": sum(int(x) << i for i, x in enumerate(b))})
    grids = []
    for v in range(1, 41):
        for e in ECCS if v <= 2 else [QrCode.Ecc.MEDIUM]:
            q = bare(v, e)
            q._draw_function_patterns()
            rec = {"version": v, "ecc": ECC_NAME[e.ordinal],
                   "modules_fnv1a": fnv1a(bytes.fromhex(pack(q._modules))),
                   "function_fnv1a": fnv1a(bytes.fromhex(pack(q._isfunction)))}
            if v <= 7 or v == 40:
                rec["modules"], rec["function"] = pack(q._modules), pack(q._isfunction)
            grids.append(rec)
    return {"format_bits": fmt, "version_bits": vers, "grids": grids}


def masks():
    """Each mask of a few symbols: the grid after masking and format bits, and its penalty."""
    cases = []
    for v, e in [(1, 0), (2, 1), (4, 1), (7, 2), (10, 3), (25, 0), (40, 1)]:
        ecl = ECCS[e]
        data = payload(QrCode._get_num_data_codewords(v, ecl))
        q = bare(v, ecl)
        q._draw_function_patterns()
        q._draw_codewords(q._add_ecc_and_interleave(bytearray(data)))
        unmasked = pack(q._modules)
        per = []
        for m in range(8):
            q._apply_mask(m)
            q._draw_format_bits(m)
            g = pack(q._modules)
            rec = {"mask": m, "penalty": q._get_penalty_score(), "fnv1a": fnv1a(bytes.fromhex(g))}
            if v <= 4:
                rec["modules"] = g
            per.append(rec)
            q._apply_mask(m)
        chosen = QrCode(v, ecl, data, -1)
        cases.append({"version": v, "ecc": ECC_NAME[e], "data": data.hex(),
                      "unmasked_fnv1a": fnv1a(bytes.fromhex(unmasked)), "masks": per, "chosen": chosen.get_mask()})
    return {"cases": cases}


def symbol_json(q, **kw):
    g = [[q.get_module(x, y) for x in range(q.get_size())] for y in range(q.get_size())]
    return {**kw, "version": q.get_version(), "ecc_out": ECC_NAME[q.get_error_correction_level().ordinal],
            "mask_out": q.get_mask(), "modules": pack(g)}


def symbols():
    out = []
    def enc(kind, value, segs, e, minv=1, maxv=40, mask=-1, boost=True):
        try:
            q = QrCode.encode_segments(segs, ECCS[e], minv, maxv, mask, boost)
            out.append(symbol_json(q, kind=kind, input=value, ecc=ECC_NAME[e], min_version=minv,
                                   max_version=maxv, mask=mask, boost_ecl=boost))
        except qrcodegen.DataTooLongError as ex:
            out.append({"kind": kind, "input": value, "ecc": ECC_NAME[e], "min_version": minv, "max_version": maxv,
                        "mask": mask, "boost_ecl": boost, "error": str(ex)})
    for i, t in enumerate(TEXTS):
        enc("text", t, QrSegment.make_segments(t), i % 4)
    for e in range(4):
        enc("text", "Hello, world!", QrSegment.make_segments("Hello, world!"), e, boost=False)
    for m in range(8):
        enc("text", "HELLO WORLD", QrSegment.make_segments("HELLO WORLD"), 2, mask=m)
    enc("text", "hello", QrSegment.make_segments("hello"), 0, minv=5, maxv=9)
    enc("text", "hello", QrSegment.make_segments("hello"), 1, minv=40)
    enc("text", "x" * 100, QrSegment.make_segments("x" * 100), 3, maxv=3)
    for n, e in [(1, 0), (100, 1), (500, 2), (1000, 3), (2000, 0), (2331, 0), (2953, 0), (2954, 0)]:
        d = payload(n)
        enc("bytes", d.hex(), [QrSegment.make_bytes(d)], e)
    d = "".join(rng.choice("0123456789") for _ in range(7089))
    enc("text", d, QrSegment.make_segments(d), 0)
    sjis, k = kanji_segment("漢字モードと点")
    enc("segments", [{"kanji": sjis.hex()}], [k], 1)
    segs = [QrSegment.make_eci(26), QrSegment.make_bytes("Grüße".encode()), QrSegment.make_numeric("0123")]
    enc("segments", [{"eci": 26}, {"bytes": "Grüße".encode().hex()}, {"numeric": "0123"}], segs, 2)
    return {"symbols": out}


def vouchers():
    """The voucher code as hushphone, the T-Deck and katzenqt show it: the code string in
    byte mode, level M, the smallest version, automatic mask, no ECC boost."""
    out = []
    for code in ["cvV8Q1uGC31DYm4pNGXfCg8ihJA1Pj7sYkyK/eBIP28=", "rYftB9lL3klTpijbh2cDQf148H1y6jsCcGTsBVG/CJo="]:
        q = QrCode.encode_segments([QrSegment.make_bytes(code.encode())], QrCode.Ecc.MEDIUM, 1, 40, -1, False)
        out.append(symbol_json(q, code=code))
    return {"vouchers": out}


def main(outdir):
    write(outdir, "reed_solomon", gf256_and_rs())
    write(outdir, "segments", segments())
    caps, inter = capacity_and_codewords()
    write(outdir, "capacity", caps)
    write(outdir, "codewords", inter)
    write(outdir, "function_patterns", function_patterns())
    write(outdir, "masks", masks())
    write(outdir, "symbols", symbols())
    write(outdir, "vouchers", vouchers())


if __name__ == "__main__":
    main(sys.argv[1])
