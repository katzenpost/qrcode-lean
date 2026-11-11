import Qrcode.Segment
import Qrcode.Symbol

/-!
The symbol type and the encoder entry points, as qrcodegen's `QrCode` factories.
-/

namespace Qrcode

/-- A finished QR Code symbol. Its grid is as wide as its version says. -/
structure QrCode where
  version : Version
  ecc : Ecc
  mask : Mask
  modules : Grid version.size

inductive Error where
  /-- `minVersion` is above `maxVersion`. -/
  | versionRange
  /-- The data does not fit `maxVersion` at the requested level. `usedBits` is
  `none` when a segment's character count overflows its field. -/
  | dataTooLong (usedBits : Option Nat) (capacityBits : Nat)
  deriving DecidableEq, Repr

instance : ToString Error where
  toString
    | .versionRange => "minimum version above maximum version"
    | .dataTooLong (some u) c => s!"Data length = {u} bits, Max capacity = {c} bits"
    | .dataTooLong none _ => "Segment too long"

structure Options where
  minVersion : Version := .min
  maxVersion : Version := .max
  /-- `none` picks the mask with the lowest penalty score. -/
  mask : Option Mask := none
  /-- Raise the level while the data still fits the chosen version. -/
  boostEcl : Bool := true

namespace QrCode

def size (q : QrCode) : Nat := q.version.size

/-- Whether the module at `(x, y)` is dark; `(0, 0)` is the top left. Coordinates
outside the symbol are light, so a quiet zone can be read off negative or large
coordinates. -/
def module (q : QrCode) (x y : Int) : Bool :=
  x ≥ 0 && y ≥ 0 && q.modules.get x.toNat y.toNat

/-- The data codewords of `segs` in version `v` at level `e`: mode headers, data,
a terminator of up to four zero bits, zero bits to a byte boundary, then pad bytes
0xEC and 0x11 by turns. `bits` must fit the capacity. -/
def dataCodewords (segs : List Segment) (v : Version) (e : Ecc) : Vector UInt8 (numDataCodewords v e) :=
  let bb := segs.foldl (init := #[]) fun bb s =>
    appendBits (appendBits bb s.mode.bits 4) s.numChars (s.mode.charCountBits v) ++ s.data
  let cap := numDataCodewords v e
  -- bytes holding data or terminator; the terminator may be cut short by the capacity
  let filled := min cap ((bb.size + 4 + 7) / 8)
  Vector.ofFn fun i =>
    if i.val < filled then
      (List.range 8).foldl (fun b k => b <<< 1 ||| (if bb.getD (8 * i.val + k) false then 1 else 0)) 0
    else if (i.val - filled) % 2 == 0 then 0xEC else 0x11

/-- The symbol for version `v`, level `e` and the given data codewords; with
`mask := none` it tries all eight masks and keeps the lowest penalty, the first on ties. -/
def ofCodewords (v : Version) (e : Ecc) (data : Vector UInt8 (numDataCodewords v e))
    (mask : Option Mask := none) : QrCode :=
  let all := addEccAndInterleave v e data
  let c := (functionPatterns v e).drawCodewords (ByteArray.mk all.toArray)
  let masked (m : Mask) := (c.applyMask m).drawFormatBits e m
  let m := mask.getD <| Prod.snd <| (List.finRange 8).foldl (init := (none, 0)) fun (best, bm) m =>
    let p := penaltyScore (masked m).modules
    match best with
    | some b => if p < b then (some p, m) else (best, bm)
    | none => (some p, m)
  ⟨v, e, m, (masked m).modules⟩

/-- Encodes `segs` in the smallest version from `minVersion` to `maxVersion` that
holds them, at level `ecl` or higher with `boostEcl`. -/
def encodeSegments (segs : List Segment) (ecl : Ecc) (opts : Options := {}) : Except Error QrCode := do
  unless opts.minVersion ≤ opts.maxVersion do throw .versionRange
  let fits (v : Version) (e : Ecc) := match Segment.totalBits segs v with
    | some b => b ≤ numDataCodewords v e * 8
    | none => false
  let some v := (Version.range opts.minVersion opts.maxVersion).find? (fits · ecl)
    | throw (.dataTooLong (Segment.totalBits segs opts.maxVersion) (numDataCodewords opts.maxVersion ecl * 8))
  let ecl := if opts.boostEcl then
      [Ecc.medium, .quartile, .high].foldl (fun e e' => if fits v e' then e' else e) ecl
    else ecl
  return ofCodewords v ecl (dataCodewords segs v ecl) opts.mask

/-- Binary data as one byte-mode segment. -/
def encodeBytes (data : ByteArray) (ecl : Ecc) (opts : Options := {}) : Except Error QrCode :=
  encodeSegments [Segment.makeBytes data] ecl opts

/-- Text in numeric, alphanumeric or UTF-8 byte mode, whichever is the narrowest
that holds all of it. -/
def encodeText (text : String) (ecl : Ecc) (opts : Options := {}) : Except Error QrCode :=
  encodeSegments (Segment.makeSegments text) ecl opts

end QrCode

end Qrcode
