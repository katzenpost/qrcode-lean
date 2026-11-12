# Test vectors

Written by `tools/gen-vectors.py` from Project Nayuki's qrcodegen 1.8.0, the
Python package on PyPI, calling the library's own steps:

```sh
pip install qrcodegen==1.8.0
python3 tools/gen-vectors.py tests/vectors
```

CI regenerates them and fails if they differ. Grids are packed row-major, most
significant bit first, as hex; large ones are given only as a 64-bit FNV-1a hash
of those bytes.

- `reed_solomon/`: GF(2^8) products, the generator polynomials of degree 1 to 30,
  and remainders for every parity length QR Codes use.
- `segments/`: numeric, alphanumeric, byte and ECI segment bits, the automatic
  mode choice for text, and bit totals per version. The Python library has no
  kanji mode; the kanji segments are computed in the generator from Shift JIS by
  ISO/IEC 18004 section 8.4.5, and full symbols from them by qrcodegen.
- `capacity/`: raw data modules, alignment pattern positions and data codewords
  for every version and level.
- `codewords/`: Reed-Solomon parity and block interleaving for all 160 versions
  and levels, from data codewords given by a formula the tests repeat.
- `function_patterns/`: format bits for every level and mask, version bits, and
  the function pattern grid of every version.
- `masks/`: for seven symbols, each mask's grid and penalty score, and the mask
  qrcodegen chooses.
- `symbols/`: whole symbols from text, bytes and mixed segments, across levels,
  version limits, fixed masks, level boost, and inputs too long to fit.
- `vouchers/`: two voucher codes as hushphone, the T-Deck and katzenqt encode
  them (byte mode, level M, smallest version, automatic mask, no boost). The C
  qrcodegen the T-Deck uses gives the same two symbols.

The decoder's vectors, `decode/`, are written by `tools/gen-decode-vectors.py`
with Daniel Beer's quirc 1.2 as the reference decoder, built from a checkout by
the system C compiler around `tools/quirc-harness.c`, which reaches quirc's own
error correction steps:

```sh
git clone --depth 1 --branch v1.2 https://github.com/dlbeer/quirc
python3 tools/gen-decode-vectors.py quirc tests/vectors
```

- Reed-Solomon blocks for every parity length, with up to two more wrong
  bytes than the parity corrects, and quirc's correction of each.
- Format bits for every level and mask with up to five wrong bits, and the
  level and mask quirc reads from them. quirc sometimes accepts a word four
  bits from the nearest code word; this decoder stops at three.
- Whole symbols from qrcodegen in each segment mode, with modules flipped at
  random, and quirc's decoding of each grid.
- Images of the voucher code and of a version 8 symbol, rendered by the
  generator as 8-bit PGM: clean, 2.5 pixels a module, turned, upside down,
  in perspective, noisy and mirrored. Each has the finder centres the renderer
  placed, and quirc's grid and decoding; quirc reads five of the eight, and the
  tests require this decoder to read all of them. The camera photos in `tests/photos/`
  are inputs; the generator records quirc's reading of them too.
