import QrcodeTests.Common

/-! Whole symbols against qrcodegen's, and the voucher codes the katzenqt clients show. -/

open Qrcode
open Lean (Json)

namespace QrcodeTests

def segmentsOf (spec : Array Json) : Option (List Segment) :=
  spec.toList.mapM fun s =>
    if s.has "kanji" then Segment.makeKanji (unhex (s.strD "kanji"))
    else if s.has "eci" then Segment.makeEci (s.natD "eci")
    else if s.has "bytes" then some (Segment.makeBytes (unhex (s.strD "bytes")))
    else Segment.makeNumeric (s.strD "numeric")

def symbolMatches (q : QrCode) (r : Json) : Bool :=
  q.version.val == r.natD "version" && toString q.ecc == r.strD "ecc_out" &&
    q.mask.val == r.natD "mask_out" && hex q.modules.toBytes == r.strD "modules"

def symbolVectors : T Unit := do
  let j ← loadJson "symbols"
  for r in j.arrD "symbols" do
    let m := r.intD "mask"
    let opts : Options := {
      minVersion := versionOf (r.natD "min_version")
      maxVersion := versionOf (r.natD "max_version")
      boostEcl := r.boolD "boost_ecl"
      mask := if m < 0 then none else some (Fin.ofNat 8 m.toNat) }
    let ecl := eccOf (r.strD "ecc")
    let (what, got) := match r.strD "kind" with
      | "text" => (s!"text {(r.strD "input").take 20}", QrCode.encodeText (r.strD "input") ecl opts)
      | "bytes" => (s!"{(r.strD "input").length / 2} bytes", QrCode.encodeBytes (unhex (r.strD "input")) ecl opts)
      | _ => ("segments", match segmentsOf (r.arrD "input") with
        | some segs => QrCode.encodeSegments segs ecl opts
        | none => .error .versionRange)
    let what := s!"{what} at {ecl}"
    match got with
    | .ok q => check (!r.has "error" && symbolMatches q r) s!"symbol {what}: v{q.version} {q.ecc} mask {q.mask}"
    | .error e => check (r.has "error" && toString e == r.strD "error") s!"symbol {what}: {e}"
  IO.println s!"  {(j.arrD "symbols").size} symbols"

/-- The voucher code as hushphone, the T-Deck and katzenqt draw it. -/
def voucherVectors : T Unit := do
  let j ← loadJson "vouchers"
  for r in j.arrD "vouchers" do
    let code := r.strD "code"
    match QrCode.encodeBytes code.toUTF8 .medium { boostEcl := false } with
    | .ok q => check (symbolMatches q r) s!"voucher {code}"
    | .error e => check false s!"voucher {code}: {e}"

/-- The renderer and the API's edges. -/
def unit : T Unit := do
  match QrCode.encodeText "hello" .low with
  | .error e => check false s!"encode hello: {e}"
  | .ok q =>
    check (q.size == 21 && q.module 0 0 && !q.module (-1) 0 && !q.module 21 0) "module bounds"
    let rows := q.toHalfBlocks
    -- 29 modules with the quiet zone: 15 rows of 29 cells, the last row half quiet zone
    check (rows.length == 15 && rows.all (·.length == 29)) "half block shape"
    check (rows.head? == some (String.ofList (List.replicate 29 ' '))) "quiet zone is blank"
    let decoded := (List.range 29).all fun y => (List.range 29).all fun x =>
      let cell := rows[y / 2]!.toList[x]!
      let dark := if y % 2 == 0 then cell == '█' || cell == '▀' else cell == '█' || cell == '▄'
      dark == q.module (x - 4 : Int) (y - 4 : Int)
    check decoded "half blocks show the modules"
    let inv := q.toHalfBlocks (border := 1) (inkDark := false)
    -- top row: quiet zone over quiet zone, then quiet zone over the finder's dark edge
    check (inv.length == 12 && inv.all (·.length == 23) && inv.head!.toList.take 2 == ['█', '▀']) "inverted ink"
  check (QrCode.encodeBytes .empty .low { minVersion := versionOf 5, maxVersion := versionOf 4 } matches .error .versionRange)
    "min above max"

end QrcodeTests
