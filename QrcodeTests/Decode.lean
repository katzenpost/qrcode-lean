import QrcodeTests.Common

/-! The decoder: each step against quirc's (`tests/vectors/decode/`, written by
`tools/gen-decode-vectors.py`), grids against the encoder, and whole images. -/

open Qrcode Qrcode.Decode
open Lean (Json)

namespace QrcodeTests

/-- A grid from rows packed most significant bit first, as hex. -/
def gridOf (n : Nat) (packed : String) : Grid n :=
  let b := unhex packed
  .ofFn fun x y => (b[(y * n + x) / 8]!.toNat >>> (7 - (y * n + x) % 8)) % 2 == 1

def versionOfSize (n : Nat) : Option Version :=
  (Version.ofNat? ((n - 17) / 4)).filter (·.size == n)

/-- Whether `got` is what a quirc result `r` says: its version, level, mask and payload,
or an error if quirc gave one. -/
def matchesQuirc (got : Except String Decoded) (r : Json) : Bool :=
  match got with
  | .ok d => !r.has "error" && d.version.val == r.natD "version" && toString d.ecc == r.strD "ecc" &&
    d.mask.val == r.natD "mask" && hex d.payload == r.strD "payload"
  | .error _ => r.has "error"

def rsDecodeVectors : T Unit := do
  let j ← loadJson "decode"
  for r in j.arrD "reed_solomon" do
    let got := (ReedSolomon.correct (unhex (r.strD "block")) (r.natD "parity")).map hex
    check (got == if r.has "corrected" then some (r.strD "corrected") else none)
      s!"RS {r.natD "parity"} parity bytes, {r.natD "errors"} errors: {got}"
  IO.println s!"  {(j.arrD "reed_solomon").size} blocks"

/-- Every pattern of up to three set bits below bit `w`. -/
def errorPatterns (w : Nat) : List Nat :=
  let r := List.range w
  let after (i : Nat) := r.filter (i < ·)
  [0] ++ r.map (1 <<< ·) ++ (r.flatMap fun i => (after i).map fun j => 1 <<< i ||| 1 <<< j) ++
    (r.flatMap fun i => (after i).flatMap fun j => (after j).map fun k => 1 <<< i ||| 1 <<< j ||| 1 <<< k)

def formatVectors : T Unit := do
  let j ← loadJson "decode"
  for r in j.arrD "format" do
    let want := if r.has "ecc" then some (r.strD "ecc", r.natD "mask") else none
    let got := (decodeFormat [r.natD "bits"]).map fun (e, m) => (toString e, m.val)
    -- quirc's BCH decoder sometimes lands on a code word four bits away, past the three the
    -- code guarantees (both cases here tie with another word); we stop at three
    let tie := got.isNone && Ecc.all.any fun e => (List.finRange 8).any fun m =>
      want == some (toString e, m.val) && popcount (r.natD "bits" ^^^ formatBits e m) == 4
    check (got == want || tie) s!"format bits {r.natD "bits"} with {r.natD "errors"} errors: {got}"
  -- every code word with every pattern of up to three wrong bits
  let pats15 := errorPatterns 15
  let pats18 := errorPatterns 18
  check (pats15.length == 1 + 15 + 105 + 455 && pats18.length == 1 + 18 + 153 + 816) "error patterns"
  for e in Ecc.all do
    for m in List.finRange 8 do
      check (pats15.all fun p => decodeFormat [formatBits e m ^^^ p, 0] == some (e, m)) s!"format {e} {m}"
  for v in Version.all.filter (7 ≤ ·.val) do
    check (pats18.all fun p => decodeVersion [0, versionBits v ^^^ p] == some v) s!"version {v}"
  IO.println s!"  {(j.arrD "format").size} format words, and every word with up to 3 bits wrong"

/-- Every version, level and mask, from the encoder and back, in byte mode; then the
other modes. -/
def gridRoundTrip : T Unit := do
  let mut n := 0
  for v in Version.all do
    for e in Ecc.all do
      let cap := (numDataCodewords v e * 8 - 4 - Mode.byte.charCountBits v) / 8
      for m in List.finRange 8 do
        let data : ByteArray := ⟨(Array.range (cap * (m.val + 1) / 8)).map fun i => (i * 31 + v.val * 7 + m.val).toUInt8⟩
        match QrCode.encodeBytes data e { minVersion := v, maxVersion := v, mask := some m, boostEcl := false } with
        | .error err => check false s!"encode v{v}-{e} mask {m}: {err}"
        | .ok q =>
          n := n + 1
          let ok := match decodeGrid q.version q.modules with
            | .ok d => d.payload == data && d.version == v && d.ecc == e && d.mask == m
            | .error _ => false
          check ok s!"round trip v{v}-{e} mask {m}"
  -- numeric, alphanumeric, kanji (back as Shift JIS) and ECI, alone and mixed, at each count field width
  let digits := "31415926535897932384626433832795028841971"
  let kanji := unhex "8abf8e9a8382815b836882c6935f"
  let segs : List (List Segment × String) := [
    ([(Segment.makeNumeric digits).get!], digits),
    ([(Segment.makeNumeric "7").get!, (Segment.makeNumeric "42").get!], "742"),
    ([(Segment.makeAlphanumeric "KATZENPOST $%*+-./:").get!], "KATZENPOST $%*+-./:"),
    ([(Segment.makeAlphanumeric "A").get!], "A"),
    ([(Segment.makeEci 26).get!, Segment.makeBytes "Grüße".toUTF8, (Segment.makeNumeric "0123").get!], "Grüße0123"),
    ([(Segment.makeEci 1000).get!, (Segment.makeEci 999999).get!, Segment.makeBytes "x".toUTF8], "x")]
  for vn in [1, 9, 10, 26, 27, 40] do
    let v := versionOf vn
    let payload (s : List Segment) := match QrCode.encodeSegments s .low { minVersion := v, maxVersion := v } with
      | .ok q => (decodeGrid q.version q.modules).toOption.map (·.payload)
      | .error _ => none
    for (s, want) in segs do
      check (payload s == some want.toUTF8) s!"v{v} segments {want}"
    check (payload [(Segment.makeKanji kanji).get!] == some kanji) s!"v{v} kanji"
  IO.println s!"  {n} byte-mode symbols, every version, level and mask"

/-- Segment parsing on its own: truncated and unsupported data. -/
def payloadUnit : T Unit := do
  let v := versionOf 1
  -- byte mode, two bytes "hi", terminator
  check ((parsePayload v (unhex "40268690")).toOption == some "hi".toUTF8) "byte segment"
  -- byte mode, count 255, but only a few bytes follow
  check (parsePayload v (unhex "4ff00000") matches .error _) "segment past the end"
  -- structured append (mode 3) is not supported
  check (parsePayload v (unhex "30000000") matches .error _) "unsupported mode"
  -- numeric, three digits, group value 1000
  check (parsePayload v (unhex "100fe800") matches .error _) "bad numeric group"
  -- an all-zero stream is just a terminator
  check ((parsePayload v (unhex "0000")).toOption == some .empty) "terminator only"

def gridVectors : T Unit := do
  let j ← loadJson "decode"
  for r in j.arrD "grids" do
    let n := r.natD "size"
    match versionOfSize n with
    | none => check false s!"grid size {n}"
    | some v =>
      let got := decodeEither v (gridOf v.size (r.strD "modules"))
      check (matchesQuirc got r) s!"v{v} grid with {r.natD "flips"} modules flipped: {got.toOption.map (hex ·.payload |>.take 16)}"
  IO.println s!"  {(j.arrD "grids").size} grids"

/-- The bitmap `decodeImage` decodes from: the frame's own, or the smoothed frame's. -/
def decodedBitmap (img : Image) : Image :=
  match decodeBitmap (binarize img) with
  | .ok _ => binarize img
  | .error _ => binarize (smooth img)

def near (f : Finder) (p : Json) : Bool :=
  let c := p.getArr?.toOption.getD #[]
  let coord (i : Nat) : Float := ((c[i]!.getNum?).toOption.map (·.toFloat)).getD 0
  (f.x - coord 0).abs < 1 && (f.y - coord 1).abs < 1

def imageVectors : T Unit := do
  let j ← loadJson "decode"
  for r in j.arrD "images" do
    let name := r.strD "name"
    let (w, h, px) ← readPgm (vectorsDir / r.strD "file")
    let some img := Image.ofBytes? w h px | check false s!"{name}: size"
    match decodeGray w h px with
    | .ok p => check (p == (r.strD "text").toUTF8) s!"{name}: payload {String.fromUTF8? p}"
    | .error e => check false s!"{name}: {e}"
    let bm := decodedBitmap img
    let ts := triples (findFinders bm)
    if r.has "finders" then
      -- the renderer's finder centres: top left, top right, bottom left; a mirror swaps the last two
      let fs := r.arrD "finders"
      check (ts.head?.any fun (bl, tl, tr) =>
          near tl fs[0]! && (near tr fs[1]! && near bl fs[2]! || near tr fs[2]! && near bl fs[1]!))
        s!"{name}: finders {ts.head?.map fun (bl, tl, tr) => [(tl.x, tl.y), (tr.x, tr.y), (bl.x, bl.y)]}"
    for c in r.arrD "quirc" do
      let some v := versionOfSize (c.natD "size") | check false s!"{name}: quirc grid size"
      let qg := gridOf v.size (c.strD "modules")
      check (matchesQuirc (decodeEither v qg) c) s!"{name}: quirc's grid"
      -- our grid of the same symbol, module by module against quirc's
      let some (bl, tl, tr) := ts.head? | check false s!"{name}: no finders"
      let g := sampleSymbol bm bl tl tr v
      let diffs := ((List.range v.size).flatMap fun y => (List.range v.size).filter fun x => g.get x y != qg.get x y).length
      IO.println s!"  {name}: {diffs} of {v.size * v.size} modules differ from quirc's grid"
      check (diffs * 20 ≤ v.size * v.size) s!"{name}: {diffs} modules differ from quirc's"
  IO.println s!"  {(j.arrD "images").size} images"

/-- A 640 x 480 frame: the voucher symbol at 8 pixels a module on a lit page in a
darker room, or with `symbol := false` the page alone. `seed` sets one pixel, so each
frame is decoded afresh. -/
def benchFrame (q : QrCode) (seed : Nat) (symbol := true) : Image :=
  .ofFn 640 480 fun x y =>
    let (mx, my) := ((x : Int) - 188, (y : Int) - 108)
    if x == 0 && y == 0 then seed.toUInt8
    else if mx < -40 || my < -40 || mx ≥ 304 || my ≥ 304 then (40 + (x + y) / 16).toUInt8
    else if symbol && mx ≥ 0 && my ≥ 0 && q.module (mx / 8) (my / 8) then 30
    else (200 + x / 40).toUInt8

def bench : T Unit := do
  let .ok q := QrCode.encodeBytes "cvV8Q1uGC31DYm4pNGXfCg8ihJA1Pj7sYkyK/eBIP28=".toUTF8 .medium { boostEcl := false }
    | check false "encode"
  for (what, symbol) in [("with the voucher symbol", true), ("with no symbol", false)] do
    let frames := (List.range 10).map (benchFrame q · symbol)
    let t0 ← IO.monoNanosNow
    let mut found := 0
    for f in frames do
      if let .ok p := decodeGray f.width f.height f.pixels then
        found := found + 1
        check (p == "cvV8Q1uGC31DYm4pNGXfCg8ihJA1Pj7sYkyK/eBIP28=".toUTF8) "bench payload"
    let ms := ((← IO.monoNanosNow) - t0).toFloat / 1e6 / frames.length.toFloat
    check (found == if symbol then frames.length else 0) s!"bench {what}: {found} decoded"
    IO.println s!"  decodeGray on a 640x480 frame {what}: {ms.round.toUInt64} ms"
  -- the entry point checks the pixel count
  check (decodeGray 4 4 (ByteArray.mk (Array.replicate 15 0)) matches .error _) "short frame"
  check (decodeGray 0 0 .empty matches .error _) "empty frame"

end QrcodeTests
