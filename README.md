# qrcode-lean

A QR Code encoder in pure Lean 4, for Lean programs that need to show data
as a QR Code. It is a port of Project Nayuki's
[QR Code generator library](https://www.nayuki.io/page/qr-code-generator-library)
(qrcodegen) and draws the same symbol as qrcodegen, module for module, for the
same input and options.

It covers QR Code Model 2: versions 1 to 40, the four error correction levels,
numeric, alphanumeric, byte, kanji and ECI segments, and the eight masks with
qrcodegen's penalty rule. The encoder is a set of total functions from bytes to
a module grid, with no IO, no FFI and no dependencies beyond Lean core. It does
not decode QR Codes, draw Micro QR or structured append symbols, or write
images; it returns the grid and a text rendering for terminals. Programs that
need those should use qrcodegen or another library.

This package was ported with the help of a large language model, and is
checked against qrcodegen with test vectors.

## Use

Add it to your `lakefile.toml`:

```toml
[[require]]
name = "qrcode"
git = "https://github.com/katzenpost/qrcode-lean"
rev = "main"
```

A voucher code as the Katzenpost clients show it: byte mode, level M, the
smallest version that holds it, no level boost, printed with half blocks.

```lean
import Qrcode
open Qrcode

def code := "cvV8Q1uGC31DYm4pNGXfCg8ihJA1Pj7sYkyK/eBIP28="

def symbol : Except Error QrCode := QrCode.encodeBytes code.toUTF8 .medium { boostEcl := false }

-- q.size modules on a side; q.module x y is true for dark, false outside the symbol
def main : IO Unit :=
  match symbol with
  | .ok q => (q.toHalfBlocks (border := 4)).forM IO.println
  | .error _ => IO.eprintln "too long for a QR Code"
```

`Options` also takes `minVersion`, `maxVersion` and a fixed `mask`.
`QrCode.encodeText` picks numeric, alphanumeric or UTF-8 byte mode, and
`QrCode.encodeSegments` takes segments built with `Segment.make*`.

## Map

- [`Qrcode/`](Qrcode): `GF256` and `ReedSolomon` (error correction), `Version`
  (versions, levels, capacities), `Segment` (modes), `Grid` and `Symbol`
  (patterns, codeword placement, masks), `Encode` (the `encode*` entry points),
  `Render` (half blocks for terminals).
- [`QrcodeTests/`](QrcodeTests), [`tests/vectors/`](tests/vectors/README.md) and
  [`tools/gen-vectors.py`](tools/gen-vectors.py): the tests, their vectors, and
  the script that writes the vectors from qrcodegen.

## Status and tests

Version 0.1.0, built with Lean `v4.35.0` (`lean-toolchain`).

```sh
lake build
lake test
```

`lake test` checks each step of the encoder (Reed-Solomon, segments, codeword
interleaving, function patterns, format and version bits, each mask and its
penalty) and whole symbols against vectors from qrcodegen 1.8.0, among them the
voucher codes hushphone, the T-Deck and katzenqt show. CI regenerates the
vectors and fails if they differ;
[`tests/vectors/README.md`](tests/vectors/README.md) says how. Issues and pull
requests are welcome on GitHub.

## Licence

MIT. The ported code keeps Project Nayuki's copyright notice; see
[`LICENSE`](LICENSE).
