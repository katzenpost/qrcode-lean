import Qrcode.Segment

/-!
Segment parsing: the data codewords back to the bytes the segments carry.
-/

namespace Qrcode.Decode

/-- `k` bits of `data` from bit `pos`, most significant first. -/
def takeBits (data : ByteArray) (pos k : Nat) : Nat :=
  (List.range k).foldl (init := 0) fun acc i =>
    acc * 2 + (data[(pos + i) / 8]!.toNat >>> (7 - (pos + i) % 8)) % 2

/-- Bits a segment of `count` characters takes after its header. -/
def dataBits (m : Mode) (count : Nat) : Nat :=
  match m with
  | .numeric => 10 * (count / 3) + [0, 4, 7][count % 3]!
  | .alphanumeric => 11 * (count / 2) + 6 * (count % 2)
  | .byte => 8 * count
  | .kanji => 13 * count
  | .eci => 0

/-- The digits of `val`, `k` of them with leading zeros, in ASCII. -/
def pushDigits (out : ByteArray) (val k : Nat) : ByteArray :=
  (List.range k).foldl (fun out i => out.push (48 + val / 10 ^ (k - 1 - i) % 10).toUInt8) out

/-- The payload of the data codewords `data` of a version `v` symbol: each segment's
characters, digits and alphanumeric characters in ASCII and kanji in Shift JIS, up
to the terminator or the end of the data. ECI designators are read and dropped;
structured append and FNC1 are not supported. -/
def parsePayload (v : Version) (data : ByteArray) : Except String ByteArray := do
  let total := data.size * 8
  let charset := Segment.alphanumericCharset.toUTF8
  let mut pos := 0
  let mut out := ByteArray.empty
  -- each segment takes at least its four mode bits
  for _ in [0:total / 4 + 1] do
    if pos + 4 > total then break
    let ind := takeBits data pos 4
    pos := pos + 4
    if ind == 0 then break
    if ind == Mode.eci.bits then
      -- the designator is one, two or three bytes, by its leading bits
      let len := if pos + 8 > total then 0 else
        let b := takeBits data pos 8
        if b < 0x80 then 8 else if b < 0xC0 then 16 else if b < 0xE0 then 24 else 0
      if len == 0 || pos + len > total then throw "bad ECI designator"
      pos := pos + len
      continue
    let some mode := [Mode.numeric, .alphanumeric, .byte, .kanji].find? (·.bits == ind)
      | throw s!"unsupported mode {ind}"
    let cc := mode.charCountBits v
    if pos + cc > total then throw "segment header past the end of the data"
    let count := takeBits data pos cc
    pos := pos + cc
    if pos + dataBits mode count > total then throw "segment past the end of the data"
    match mode with
    | .numeric =>
      for g in [0:(count + 2) / 3] do
        let k := min 3 (count - 3 * g)
        let val := takeBits data pos (3 * k + 1)
        pos := pos + 3 * k + 1
        if val ≥ 10 ^ k then throw s!"bad numeric group {val}"
        out := pushDigits out val k
    | .alphanumeric =>
      for g in [0:(count + 1) / 2] do
        let k := min 2 (count - 2 * g)
        let val := takeBits data pos (5 * k + 1)
        pos := pos + 5 * k + 1
        if val ≥ 45 ^ k then throw s!"bad alphanumeric group {val}"
        if k == 2 then out := out.push charset[val / 45]!
        out := out.push charset[val % 45]!
    | .byte =>
      for _ in [0:count] do
        out := out.push (takeBits data pos 8).toUInt8
        pos := pos + 8
    | _ =>
      -- kanji: the inverse of `Segment.kanjiValue`
      for _ in [0:count] do
        let d := takeBits data pos 13
        pos := pos + 13
        let c := (d / 0xC0) <<< 8 ||| d % 0xC0
        let sjis := if c + 0x8140 ≤ 0x9FFC then c + 0x8140 else c + 0xC140
        out := (out.push (sjis >>> 8).toUInt8).push sjis.toUInt8
  return out

end Qrcode.Decode
