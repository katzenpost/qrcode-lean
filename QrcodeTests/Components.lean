import QrcodeTests.Common

/-! Each step of the encoder against qrcodegen's own steps
(`tests/vectors/<component>/`, written by `tools/gen-vectors.py`). -/

open Qrcode
open Lean (Json)

namespace QrcodeTests

def reedSolomon : T Unit := do
  let j ← loadJson "reed_solomon"
  for r in j.arrD "mul" do
    let v := r.nats
    check ((GF256.mk v[0]!.toUInt8 * ⟨v[1]!.toUInt8⟩).val.toNat == v[2]!) s!"GF mul {v}"
  for d in [1:31] do
    let div := ReedSolomon.divisor d
    check (hex ⟨div.toArray.map (·.val)⟩ == (j.getObjValD "divisors").strD (toString d)) s!"RS divisor {d}"
  for r in j.arrD "remainders" do
    let d := r.natD "degree"
    let rem := ReedSolomon.remainder (unhex (r.strD "data")) (ReedSolomon.divisor d)
    check (hex ⟨rem.toArray.map (·.val)⟩ == r.strD "remainder") s!"RS remainder degree {d}"
  IO.println s!"  {(j.arrD "mul").size} products, 30 divisors, {(j.arrD "remainders").size} remainders"

def segments : T Unit := do
  let j ← loadJson "segments"
  for r in j.arrD "numeric" do
    check ((Segment.makeNumeric (r.strD "text")).any (segmentIs · r)) s!"numeric {r.strD "text"}"
  for r in j.arrD "alphanumeric" do
    check ((Segment.makeAlphanumeric (r.strD "text")).any (segmentIs · r)) s!"alphanumeric {r.strD "text"}"
  for r in j.arrD "bytes" do
    check (segmentIs (Segment.makeBytes (unhex (r.strD "data"))) r) s!"bytes {r.strD "data"}"
  for r in j.arrD "eci" do
    check ((Segment.makeEci (r.natD "value")).any (segmentIs · r)) s!"eci {r.natD "value"}"
  for r in j.arrD "kanji" do
    let got := Segment.makeKanji (unhex (r.strD "sjis"))
    check (if r.has "segment" then got.isNone else got.any (segmentIs · r)) s!"kanji {r.strD "sjis"}"
  for r in j.arrD "make_segments" do
    let want := r.arrD "segments"
    let got := Segment.makeSegments (r.strD "text")
    check (got.length == want.size && (got.zip want.toList).all fun (s, w) => segmentIs s w)
      s!"makeSegments {r.strD "text"}"
  for r in j.arrD "total_bits" do
    let segs := [Segment.makeBytes (ByteArray.mk (Array.replicate (r.natD "byte_len") 0))]
    let want := (r.getObjValD "bits").getArr?.toOption.getD #[] |>.map fun b => (b.getNat?).toOption
    check (Version.all.map (Segment.totalBits segs) == want.toList) s!"totalBits {r.natD "byte_len"} bytes"
  -- out of range inputs
  check (Segment.makeNumeric "12a").isNone "numeric rejects letters"
  check (Segment.makeAlphanumeric "abc").isNone "alphanumeric rejects lower case"
  check (Segment.makeEci 1000000).isNone "ECI rejects 10^6"

def capacity : T Unit := do
  let j ← loadJson "capacity"
  for r in j.arrD "capacity" do
    let v := versionOf (r.natD "version")
    check (v.rawDataModules == r.natD "raw_modules") s!"raw modules v{v}"
    check (v.alignmentPositions.toArray == (r.getObjValD "alignment").nats) s!"alignment v{v}"
    check ((Ecc.all.map (numDataCodewords v)).toArray == (r.getObjValD "data_codewords").nats) s!"data codewords v{v}"

/-- The interleave inputs, by the formula in `tools/gen-vectors.py`. -/
def formulaData (v : Version) (e : Ecc) : Vector UInt8 (numDataCodewords v e) :=
  Vector.ofFn fun i => (i.val * 151 + v.val * 7 + e.ordinal.val * 53).toUInt8

def codewords : T Unit := do
  let j ← loadJson "codewords"
  for r in j.arrD "interleave" do
    let (v, e) := (versionOf (r.natD "version"), eccOf (r.strD "ecc"))
    let out := ByteArray.mk (addEccAndInterleave v e (formulaData v e)).toArray
    check (fnv1a out == r.strD "fnv1a" && (!r.has "codewords" || hex out == r.strD "codewords"))
      s!"interleave v{v}-{e}"
  IO.println s!"  {(j.arrD "interleave").size} version and level pairs"

def functionPatternVectors : T Unit := do
  let j ← loadJson "function_patterns"
  for r in j.arrD "format_bits" do
    let m : Mask := Fin.ofNat 8 (r.natD "mask")
    check (formatBits (eccOf (r.strD "ecc")) m == r.natD "bits") s!"format bits {r.strD "ecc"} {m}"
  for r in j.arrD "version_bits" do
    check (versionBits (versionOf (r.natD "version")) == r.natD "bits") s!"version bits {r.natD "version"}"
  for r in j.arrD "grids" do
    let (v, e) := (versionOf (r.natD "version"), eccOf (r.strD "ecc"))
    let c := functionPatterns v e
    let (m, f) := (c.modules.toBytes, c.isFunction.toBytes)
    check (fnv1a m == r.strD "modules_fnv1a" && (!r.has "modules" || hex m == r.strD "modules"))
      s!"function pattern modules v{v}-{e}"
    check (fnv1a f == r.strD "function_fnv1a" && (!r.has "function" || hex f == r.strD "function"))
      s!"function module map v{v}-{e}"

def maskVectors : T Unit := do
  let j ← loadJson "masks"
  for r in j.arrD "cases" do
    let (v, e) := (versionOf (r.natD "version"), eccOf (r.strD "ecc"))
    let some data := codewordsOf v e (unhex (r.strD "data")) | check false s!"mask data v{v}-{e}"
    let unmasked := (functionPatterns v e).drawCodewords ⟨(addEccAndInterleave v e data).toArray⟩
    check (fnv1a unmasked.modules.toBytes == r.strD "unmasked_fnv1a") s!"codewords placed v{v}-{e}"
    for mr in r.arrD "masks" do
      let m : Mask := Fin.ofNat 8 (mr.natD "mask")
      let g := (QrCode.ofCodewords v e data (some m)).modules
      let b := g.toBytes
      check (fnv1a b == mr.strD "fnv1a" && (!mr.has "modules" || hex b == mr.strD "modules")) s!"mask {m} v{v}-{e}"
      check (penaltyScore g == mr.natD "penalty") s!"penalty mask {m} v{v}-{e}: {penaltyScore g}"
    check ((QrCode.ofCodewords v e data).mask.val == r.natD "chosen") s!"chosen mask v{v}-{e}"

end QrcodeTests
