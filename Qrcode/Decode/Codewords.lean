import Qrcode.Symbol

/-!
From modules to data codewords: unmasking and reading the zigzag, splitting the
interleaved blocks apart, and correcting each block.
-/

namespace Qrcode.Decode

/-- The codewords in grid `g` of version `v`, unmasked with `m`, in placement order;
the remainder bits after the last whole codeword are dropped. -/
def readCodewords (v : Version) (g : Grid v.size) (m : Mask) : ByteArray := Id.run do
  -- the format bits drawn with the function patterns do not change which modules are function modules
  let order := codewordOrder (functionPatterns v .low).isFunction
  let mut out := ByteArray.emptyWithCapacity v.rawCodewords
  let mut byte : UInt8 := 0
  for i in [0:v.rawCodewords * 8] do
    let (x, y) := order[i]!
    byte := byte <<< 1 ||| (if g.get x y ^^ m.inverts x y then 1 else 0)
    if i % 8 == 7 then
      out := out.push byte
      byte := 0
  return out

/-- The blocks of interleaved codewords `raw`, each data then parity: the inverse of
the interleaving in `addEccAndInterleave`. -/
def deinterleave (v : Version) (e : Ecc) (raw : ByteArray) : Array ByteArray := Id.run do
  let nb := numBlocks v e
  let eccLen := eccPerBlock v e
  let numShort := nb - v.rawCodewords % nb
  let shortLen := v.rawCodewords / nb
  let mut blocks := Array.replicate nb ByteArray.empty
  let mut k := 0
  for i in [0:shortLen + 1] do
    for j in [0:nb] do
      if i != shortLen - eccLen || j ≥ numShort then
        blocks := blocks.modify j (·.push raw[k]!)
        k := k + 1
  return blocks

/-- The data codewords of `blocks`, each corrected; `none` if a block has more
errors than its parity corrects. -/
def correctBlocks (v : Version) (e : Ecc) (blocks : Array ByteArray) : Option ByteArray :=
  let eccLen := eccPerBlock v e
  blocks.foldlM (init := ByteArray.empty) fun out b => do
    let c ← ReedSolomon.correct b eccLen
    return out ++ c.extract 0 (c.size - eccLen)

end Qrcode.Decode
