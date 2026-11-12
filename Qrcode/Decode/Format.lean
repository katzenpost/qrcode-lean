import Qrcode.Symbol

/-!
Format and version information. Both are short codes with few code words (32
format words of BCH(15,5), minimum distance 7; 34 version words of Golay(18,6),
minimum distance 8), so the decoder tries every code word and keeps the nearest;
that corrects up to three wrong bits, as many as either code guarantees.
-/

namespace Qrcode.Decode

/-- Set bits in `x`. -/
def popcount (x : Nat) : Nat := if h : x = 0 then 0 else x % 2 + popcount (x / 2)
decreasing_by omega

/-- The candidate whose code word is nearest to one of `words`, if within three bits. -/
def nearest (cands : List α) (code : α → Nat) (words : List Nat) : Option α :=
  Prod.fst <| cands.foldl (init := (none, 4)) fun (best, d) c =>
    let d' := words.foldl (fun m w => min m (popcount (w ^^^ code c))) d
    if d' < d then (some c, d') else (best, d)

/-- The level and mask of the format bits nearest to `words`, as read off the symbol
(still XORed with 0x5412). -/
def decodeFormat (words : List Nat) : Option (Ecc × Mask) :=
  nearest (Ecc.all.flatMap fun e => (List.finRange 8).map (e, ·)) (fun (e, m) => formatBits e m) words

/-- The version of the version bits nearest to `words`; versions 7 to 40 carry them. -/
def decodeVersion (words : List Nat) : Option Version :=
  nearest (Version.all.filter (7 ≤ ·.val)) versionBits words

/-- `k` bits read from `g`, bit `i` from the module at `pos i`. -/
def readBits (g : Grid n) (k : Nat) (pos : Nat → Nat × Nat) : Nat :=
  (List.range k).foldl (fun acc i => if g.get (pos i).1 (pos i).2 then acc ||| 1 <<< i else acc) 0

/-- Both copies of the format bits. -/
def formatWords (g : Grid n) : List Nat :=
  [readBits g 15 (formatPositions n · |>.1), readBits g 15 (formatPositions n · |>.2)]

/-- Both copies of the version bits, beside the top right and bottom left finders. -/
def versionWords (g : Grid n) : List Nat :=
  [readBits g 18 fun i => (n - 11 + i % 3, i / 3), readBits g 18 fun i => (i / 3, n - 11 + i % 3)]

end Qrcode.Decode
