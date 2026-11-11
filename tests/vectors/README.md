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
