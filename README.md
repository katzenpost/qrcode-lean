# qrcode-lean

A QR Code encoder and decoder in Lean 4. The encoder is ported from Project
Nayuki's [QR Code generator library](https://www.nayuki.io/page/qr-code-generator-library)
(qrcodegen). It covers QR Code Model 2: versions 1 to 40, the four error
correction levels, numeric, alphanumeric, byte, kanji and ECI segments, and
the eight masks with qrcodegen's penalty rule. The decoder reads the first
symbol in an 8-bit grayscale image, such as a camera frame.

Both are pure by design: total functions with no IO, no FFI and no
dependencies beyond Lean core; a camera is the caller's business. The encoder
draws the same symbol as qrcodegen, module for module, for the same input and
options. Versions, levels and masks are types; a symbol's grid is indexed by
its size, and the data codewords by the capacity of its version and level.

This package was written with the help of a large language model, and is
checked against qrcodegen and quirc with test vectors.

## Example

A voucher code as the Katzenpost clients show it: byte mode, level M, the
smallest version that holds it, no level boost.

```lean
import Qrcode
open Qrcode

def code := "cvV8Q1uGC31DYm4pNGXfCg8ihJA1Pj7sYkyK/eBIP28="

def symbol : Except Error QrCode := QrCode.encodeBytes code.toUTF8 .medium { boostEcl := false }

-- q.size modules on a side; q.module x y is true for dark, false outside the symbol
def rows (q : QrCode) : List String := q.toHalfBlocks (border := 4)
```

`Options` also takes `minVersion`, `maxVersion` and a fixed `mask`.
`QrCode.encodeText` picks numeric, alphanumeric or UTF-8 byte mode, and
`QrCode.encodeSegments` takes segments built with `Segment.make*`.

Decoding a camera frame, `width * height` bytes of gray, row-major:

```lean
def scan (width height : Nat) (frame : ByteArray) : Except String ByteArray :=
  Decode.decodeGray width height frame
```

Each step is its own module under `Qrcode/Decode/`, and `Decode.decodeGrid`
decodes a module grid by itself. [docs/decoder.md](docs/decoder.md) describes
the method, its speed and its limits.

## Tests

`lake test` checks each step of the encoder (Reed-Solomon, segments, codeword
interleaving, function patterns, format and version bits, each mask and its
penalty) and whole symbols against qrcodegen, and the decoder's error
correction, format bits, sampled grids and images against quirc; every
version, level and mask goes through the encoder and back.
`tests/vectors/README.md` describes the vectors and how to regenerate them.

## Licence

MIT. The ported code keeps Project Nayuki's copyright notice; see `LICENSE`.
