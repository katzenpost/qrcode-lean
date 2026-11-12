import Lean.Data.Json
import Qrcode

/-! Test helpers: a failure counter, hex, hashing and JSON access. -/

open Qrcode
open Lean (Json)

namespace QrcodeTests

/-- Tests run with a counter of failed checks. -/
abbrev T := ReaderT (IO.Ref Nat) IO

def check (ok : Bool) (msg : String) : T Unit := do
  unless ok do
    IO.eprintln s!"FAIL: {msg}"
    (← read).modify (· + 1)

def hex (b : ByteArray) : String :=
  b.foldl (init := "") fun s x =>
    let d := fun (n : UInt8) => Char.ofNat (if n < 10 then 48 + n.toNat else 87 + n.toNat)
    (s.push (d (x >>> 4))).push (d (x &&& 15))

def unhex (s : String) : ByteArray := Id.run do
  let v := fun (c : Char) => if c.isDigit then c.toNat - '0'.toNat else c.toLower.toNat - 'a'.toNat + 10
  let cs := s.toList.toArray
  let mut b := ByteArray.empty
  for i in [0:cs.size / 2] do
    b := b.push (v cs[2 * i]! * 16 + v cs[2 * i + 1]!).toUInt8
  return b

def fnv1a (b : ByteArray) : String :=
  let h : UInt64 := b.foldl (init := 0xcbf29ce484222325) fun h x => (h ^^^ x.toUInt64) * 0x100000001b3
  hex ⟨(Array.range 8).reverse.map fun i => (h >>> (8 * i).toUInt64).toUInt8⟩

def bitString (bb : Array Bool) : String := String.ofList (bb.toList.map fun b => if b then '1' else '0')

def vectorsDir : System.FilePath := "tests" / "vectors"

def loadJson (component : String) : IO Json := do
  let path := vectorsDir / component / (component ++ ".json")
  match Json.parse (← IO.FS.readFile path) with
  | .ok j => return j
  | .error e => throw (.userError s!"{path}: {e}")

def _root_.Lean.Json.arrD (j : Json) (k : String) : Array Json := ((j.getObjValD k).getArr?).toOption.getD #[]
def _root_.Lean.Json.natD (j : Json) (k : String) : Nat := ((j.getObjValD k).getNat?).toOption.getD 0
def _root_.Lean.Json.intD (j : Json) (k : String) : Int := ((j.getObjValD k).getInt?).toOption.getD 0
def _root_.Lean.Json.strD (j : Json) (k : String) : String := ((j.getObjValD k).getStr?).toOption.getD ""
def _root_.Lean.Json.boolD (j : Json) (k : String) : Bool := ((j.getObjValD k).getBool?).toOption.getD false
def _root_.Lean.Json.has (j : Json) (k : String) : Bool := (j.getObjVal? k).toOption.isSome
def _root_.Lean.Json.nats (j : Json) : Array Nat := ((j.getArr?).toOption.getD #[]).map fun x => (x.getNat?).toOption.getD 0

def eccOf (s : String) : Ecc :=
  match s with | "L" => .low | "M" => .medium | "Q" => .quartile | _ => .high

def versionOf (n : Nat) : Version := (Version.ofNat? n).getD .min

def modeName : Mode → String
  | .numeric => "numeric" | .alphanumeric => "alphanumeric" | .byte => "byte" | .kanji => "kanji" | .eci => "eci"

/-- Whether segment `s` is the one a vector describes. -/
def segmentIs (s : Segment) (j : Json) : Bool :=
  modeName s.mode == j.strD "mode" && s.numChars == j.natD "num_chars" && bitString s.data == j.strD "bits"

/-- A binary PGM (P5, maxval 255): width, height and pixels. -/
def readPgm (path : System.FilePath) : IO (Nat × Nat × ByteArray) := do
  let b ← IO.FS.readBinFile path
  -- four whitespace-separated header fields, then one whitespace byte
  let mut fields : Array String := #[]
  let mut cur := ""
  let mut i := 0
  while fields.size < 4 && i < b.size do
    let c := Char.ofNat b[i]!.toNat
    if c.isWhitespace then
      if cur != "" then
        fields := fields.push cur
        cur := ""
    else
      cur := cur.push c
    i := i + 1
  let (w, h) := (fields[1]!.toNat!, fields[2]!.toNat!)
  unless fields[0]! == "P5" && fields[3]! == "255" && b.size ≥ i + w * h do
    throw (.userError s!"{path}: not an 8-bit binary PGM")
  return (w, h, b.extract i (i + w * h))

/-- Data codewords from bytes, if there are as many as version `v` holds at level `e`. -/
def codewordsOf (v : Version) (e : Ecc) (b : ByteArray) : Option (Vector UInt8 (numDataCodewords v e)) :=
  if b.size == numDataCodewords v e then some (Vector.ofFn fun i => b[i.val]!) else none

end QrcodeTests
