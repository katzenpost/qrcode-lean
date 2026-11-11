import Qrcode.Version

/-!
Segments: runs of data in one encoding mode, as qrcodegen's `QrSegment`. Bits are
kept as an `Array Bool`, most significant first.
-/

namespace Qrcode

/-- Appends the low `n` bits of `val`, most significant first. -/
def appendBits (bb : Array Bool) (val n : Nat) : Array Bool := Id.run do
  let mut bb := bb
  for i in [0:n] do
    bb := bb.push (val.testBit (n - 1 - i))
  return bb

inductive Mode where
  | numeric | alphanumeric | byte | kanji | eci
  deriving DecidableEq, Repr, Inhabited

namespace Mode

/-- The 4-bit mode indicator. -/
def bits : Mode → Nat
  | numeric => 1 | alphanumeric => 2 | byte => 4 | kanji => 8 | eci => 7

/-- Width of the character count field in version `v`. -/
def charCountBits (m : Mode) (v : Version) : Nat :=
  let w : Nat × Nat × Nat := match m with
    | numeric => (10, 12, 14) | alphanumeric => (9, 11, 13) | byte => (8, 16, 16)
    | kanji => (8, 10, 12) | eci => (0, 0, 0)
  if v.val < 10 then w.1 else if v.val < 27 then w.2.1 else w.2.2

end Mode

structure Segment where
  mode : Mode
  /-- Characters for text modes, bytes for byte mode, 0 for ECI. -/
  numChars : Nat
  data : Array Bool
  deriving Repr, Inhabited

namespace Segment

def makeBytes (data : ByteArray) : Segment :=
  ⟨.byte, data.size, data.foldl (init := Array.emptyWithCapacity (8 * data.size)) (appendBits · ·.toNat 8)⟩

def isNumeric (s : String) : Bool := s.all Char.isDigit

def alphanumericCharset : String := "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:"

def alphanumericValue (c : Char) : Option Nat :=
  let i := (alphanumericCharset.toList.idxOf c)
  if i < 45 then some i else none

def isAlphanumeric (s : String) : Bool := s.all fun c => (alphanumericValue c).isSome

/-- Decimal digits, three per 10 bits; `none` if `digits` has anything else. -/
def makeNumeric (digits : String) : Option Segment := do
  guard (isNumeric digits)
  let ds := digits.toList.toArray.map fun c => c.toNat - '0'.toNat
  let mut bb := #[]
  for g in [0:(ds.size + 2) / 3] do
    let n := min 3 (ds.size - 3 * g)
    let v := (List.range n).foldl (fun a k => a * 10 + ds[3 * g + k]!) 0
    bb := appendBits bb v (3 * n + 1)
  return ⟨.numeric, ds.size, bb⟩

/-- Text over `alphanumericCharset`, two characters per 11 bits; `none` otherwise. -/
def makeAlphanumeric (text : String) : Option Segment := do
  let vs ← text.toList.toArray.mapM alphanumericValue
  let mut bb := #[]
  for g in [0:vs.size / 2] do
    bb := appendBits bb (vs[2 * g]! * 45 + vs[2 * g + 1]!) 11
  if vs.size % 2 == 1 then
    bb := appendBits bb vs[vs.size - 1]! 6
  return ⟨.alphanumeric, vs.size, bb⟩

/-- The 13-bit kanji-mode value of the Shift JIS double-byte code `hi lo`, if QR kanji
mode covers it: 0x8140 to 0x9FFC or 0xE040 to 0xEBBF, second byte 0x40 to 0xFC but
not 0x7F. -/
def kanjiValue (hi lo : UInt8) : Option Nat :=
  let (hi, lo) := (hi.toNat, lo.toNat)
  if lo < 0x40 || lo > 0xFC || lo == 0x7F then none
  else if 0x81 ≤ hi && hi ≤ 0x9F then some ((hi - 0x81) * 0xC0 + (lo - 0x40))
  else if 0xE0 ≤ hi && hi ≤ 0xEB && (hi < 0xEB || lo ≤ 0xBF) then some ((hi - 0xC1) * 0xC0 + (lo - 0x40))
  else none

/-- Kanji mode, from Shift JIS double-byte codes; `none` if `sjis` has an odd length
or a code kanji mode lacks. qrcodegen's core omits kanji (its Java `QrSegmentAdvanced`
has it, from Unicode); taking Shift JIS keeps the conversion table out of this library. -/
def makeKanji (sjis : ByteArray) : Option Segment := do
  guard (sjis.size % 2 == 0)
  let mut bb := #[]
  for i in [0:sjis.size / 2] do
    bb := appendBits bb (← kanjiValue sjis[2 * i]! sjis[2 * i + 1]!) 13
  return ⟨.kanji, sjis.size / 2, bb⟩

/-- An Extended Channel Interpretation designator; `none` unless `assignVal < 10^6`. -/
def makeEci (assignVal : Nat) : Option Segment :=
  if assignVal < 1 <<< 7 then some ⟨.eci, 0, appendBits #[] assignVal 8⟩
  else if assignVal < 1 <<< 14 then some ⟨.eci, 0, appendBits (appendBits #[] 0b10 2) assignVal 14⟩
  else if assignVal < 1000000 then some ⟨.eci, 0, appendBits (appendBits #[] 0b110 3) assignVal 21⟩
  else none

/-- One segment for `text`: numeric or alphanumeric when the whole text allows,
otherwise its UTF-8 bytes. Empty text gives no segments. -/
def makeSegments (text : String) : List Segment :=
  if text.isEmpty then []
  else (makeNumeric text <|> makeAlphanumeric text).getD (makeBytes text.toUTF8) |> ([·])

/-- Bits `segs` take in version `v`, headers included; `none` when a character
count overflows its field. -/
def totalBits (segs : List Segment) (v : Version) : Option Nat :=
  segs.foldlM (init := 0) fun acc s =>
    let cc := s.mode.charCountBits v
    if s.numChars ≥ 1 <<< cc then none else some (acc + 4 + cc + s.data.size)

end Segment

end Qrcode
