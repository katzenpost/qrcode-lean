# qrcode-lean

A QR Code encoder and decoder in pure Lean 4, for Lean programs that need to
show data as a QR Code or read one from a camera frame. The encoder is a port
of Project Nayuki's
[QR Code generator library](https://www.nayuki.io/page/qr-code-generator-library)
(qrcodegen) and draws the same symbol as qrcodegen, module for module, for the
same input and options. The decoder reads the first symbol in an 8-bit
grayscale image.

It covers QR Code Model 2: versions 1 to 40, the four error correction levels,
numeric, alphanumeric, byte, kanji and ECI segments, and the eight masks with
qrcodegen's penalty rule. Both halves are total functions with no IO, no FFI
and no dependencies beyond Lean core; a camera is the caller's business. It
does not draw or read Micro QR or structured append symbols, read light
symbols on a dark ground, or write images; the encoder returns the grid and a
text rendering for terminals. Programs that need those should use qrcodegen,
quirc or another library.

This package was written with the help of a large language model, and is
checked against qrcodegen and quirc with test vectors.

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

Decoding a camera frame, `width * height` bytes of gray, row-major:

```lean
def scan (width height : Nat) (frame : ByteArray) : Except String ByteArray :=
  Decode.decodeGray width height frame
```

`Decode.decodeGrid` decodes a module grid by itself.
[`docs/decoder.md`](docs/decoder.md) describes the method, its speed and its
limits.

## Map

- [`Qrcode/`](Qrcode): `GF256` and `ReedSolomon` (error correction), `Version`
  (versions, levels, capacities), `Segment` (modes), `Grid` and `Symbol`
  (patterns, codeword placement, masks), `Encode` (the `encode*` entry points),
  `Render` (half blocks for terminals).
- [`Qrcode/Decode/`](Qrcode/Decode): the decoder, one module per step
  (`Binarize`, `Finder`, `Perspective`, `Sample`, `Format`, `Codewords`,
  `Payload`, `Symbol`, `Image`), with `decodeGray` in
  [`Qrcode/Decode.lean`](Qrcode/Decode.lean).
- [`QrcodeTests/`](QrcodeTests), [`tests/vectors/`](tests/vectors/README.md),
  [`tools/gen-vectors.py`](tools/gen-vectors.py) and
  [`tools/gen-decode-vectors.py`](tools/gen-decode-vectors.py): the tests, their
  vectors, and the scripts that write the vectors from qrcodegen and quirc.

## Status and tests

Version 0.1.0, built with Lean `v4.35.0` (`lean-toolchain`).

```sh
lake build
lake test
```

`lake test` checks each step of the encoder (Reed-Solomon, segments, codeword
interleaving, function patterns, format and version bits, each mask and its
penalty) and whole symbols against vectors from qrcodegen 1.8.0, among them the
voucher codes hushphone, the T-Deck and katzenqt show, and the decoder's error
correction, format bits, sampled grids and images against quirc 1.2; every
version, level and mask goes through the encoder and back. CI regenerates the
vectors and fails if they differ;
[`tests/vectors/README.md`](tests/vectors/README.md) says how. Issues and pull
requests are welcome on GitHub.

## Licence

MIT. The ported code keeps Project Nayuki's copyright notice; see
[`LICENSE`](LICENSE).
