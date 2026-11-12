import Qrcode.Decode.Format
import Qrcode.Decode.Codewords
import Qrcode.Decode.Payload

/-!
Decoding a module grid: the inverse of `QrCode.ofCodewords` and the segment encoding.
-/

namespace Qrcode.Decode

/-- A decoded symbol. -/
structure Decoded where
  version : Version
  ecc : Ecc
  mask : Mask
  payload : ByteArray

/-- Decodes the modules of a version `v` symbol, `true` for dark: format information,
unmasking, codewords, error correction and segments. The version is the grid's;
version information is left to the sampler. -/
def decodeGrid (v : Version) (g : Grid v.size) : Except String Decoded := do
  let some (e, m) := decodeFormat (formatWords g) | throw "format information unreadable"
  let some data := correctBlocks v e (deinterleave v e (readCodewords v g m))
    | throw "too many errors to correct"
  return ⟨v, e, m, ← parsePayload v data⟩

end Qrcode.Decode
