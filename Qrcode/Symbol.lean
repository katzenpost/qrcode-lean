import Qrcode.Grid
import Qrcode.ReedSolomon
import Qrcode.Version

/-!
Drawing a symbol, as qrcodegen's `QrCode` constructor does: the function patterns,
the format and version information, the interleaved codewords in their zigzag,
the eight masks and the penalty rule that picks one.
-/

namespace Qrcode

/-- One of the eight data masks. -/
abbrev Mask := Fin 8

/-- Whether mask `m` inverts the module at `(x, y)`. -/
def Mask.inverts (m : Mask) (x y : Nat) : Bool :=
  match m.val with
  | 0 => (x + y) % 2 == 0
  | 1 => y % 2 == 0
  | 2 => x % 3 == 0
  | 3 => (x + y) % 3 == 0
  | 4 => (x / 3 + y / 2) % 2 == 0
  | 5 => x * y % 2 + x * y % 3 == 0
  | 6 => (x * y % 2 + x * y % 3) % 2 == 0
  | _ => ((x + y) % 2 + x * y % 3) % 2 == 0

/-- The 15 format bits for level `e` and mask `m`: BCH(15,5) code, XOR 0x5412. -/
def formatBits (e : Ecc) (m : Mask) : Nat :=
  let data := e.formatBits <<< 3 ||| m.val
  let rem := (List.range 10).foldl (fun r _ => (r <<< 1) ^^^ ((r >>> 9) * 0x537)) data
  (data <<< 10 ||| rem) ^^^ 0x5412

/-- The 18 version bits, a Golay(18,6) code; drawn from version 7 on. -/
def versionBits (v : Version) : Nat :=
  let rem := (List.range 12).foldl (fun r _ => (r <<< 1) ^^^ ((r >>> 11) * 0x1F25)) v.val
  v.val <<< 12 ||| rem

/-- The two places format bit `i` goes in a symbol `n` modules wide: around the top
left finder, and split between the other two. -/
def formatPositions (n i : Nat) : (Nat × Nat) × (Nat × Nat) :=
  let first := if i < 6 then (8, i) else if i < 8 then (8, i + 1) else if i == 8 then (7, 8) else (14 - i, 8)
  (first, if i < 8 then (n - 1 - i, 8) else (8, n - 15 + i))

/-- A symbol being drawn: its modules, and which of them are function modules. -/
structure Canvas (n : Nat) where
  modules : Grid n
  isFunction : Grid n

namespace Canvas

@[inline] def setFunction (c : Canvas n) (x y : Nat) (dark : Bool) : Canvas n :=
  -- taken apart first, so each grid is updated in place rather than copied
  let ⟨m, f⟩ := c
  ⟨m.set x y dark, f.set x y true⟩

def drawFormatBits (c : Canvas n) (e : Ecc) (m : Mask) : Canvas n := Id.run do
  let bits := formatBits e m
  let mut c := c
  for i in [0:15] do
    let (a, b) := formatPositions n i
    c := c.setFunction a.1 a.2 (bits.testBit i)
    c := c.setFunction b.1 b.2 (bits.testBit i)
  return c.setFunction 8 (n - 8) true

def drawVersion (c : Canvas n) (v : Version) : Canvas n := Id.run do
  if v.val < 7 then return c
  let bits := versionBits v
  let mut c := c
  for i in [0:18] do
    let (a, b) := (n - 11 + i % 3, i / 3)
    c := c.setFunction a b (bits.testBit i)
    c := c.setFunction b a (bits.testBit i)
  return c

/-- Chebyshev distance from 0 of `i - k`. -/
@[inline] private def dist (i k : Nat) : Nat := if i ≥ k then i - k else k - i

/-- A finder pattern centred on `(x, y)`, with its light separator. -/
def drawFinder (c : Canvas n) (x y : Nat) : Canvas n := Id.run do
  let mut c := c
  for j in [0:9] do
    for i in [0:9] do
      if x + i ≥ 4 && y + j ≥ 4 then
        let d := max (dist i 4) (dist j 4)
        c := c.setFunction (x + i - 4) (y + j - 4) (d != 2 && d != 4)
  return c

def drawAlignment (c : Canvas n) (x y : Nat) : Canvas n := Id.run do
  let mut c := c
  for j in [0:5] do
    for i in [0:5] do
      c := c.setFunction (x + i - 2) (y + j - 2) (max (dist i 2) (dist j 2) != 1)
  return c

end Canvas

/-- The function patterns of version `v`, with format bits for a placeholder mask. -/
def functionPatterns (v : Version) (e : Ecc) : Canvas v.size := Id.run do
  let n := v.size
  let mut c : Canvas n := ⟨.light n, .light n⟩
  for i in [0:n] do
    c := c.setFunction 6 i (i % 2 == 0)
    c := c.setFunction i 6 (i % 2 == 0)
  c := c.drawFinder 3 3
  c := c.drawFinder (n - 4) 3
  c := c.drawFinder 3 (n - 4)
  let pos := v.alignmentPositions
  let last := pos.length - 1
  for (i, x) in pos.zipIdx.map fun (p, i) => (i, p) do
    for (j, y) in pos.zipIdx.map fun (p, j) => (j, p) do
      unless (i == 0 && j == 0) || (i == 0 && j == last) || (i == last && j == 0) do
        c := c.drawAlignment x y
  c := c.drawFormatBits e 0
  return c.drawVersion v

/-- Splits `data` into blocks, appends each block's Reed-Solomon parity, and
interleaves the blocks' codewords. -/
def addEccAndInterleave (v : Version) (e : Ecc) (data : Vector UInt8 (numDataCodewords v e)) :
    Vector UInt8 v.rawCodewords := Id.run do
  let numBlocks := numBlocks v e
  let eccLen := eccPerBlock v e
  let raw := v.rawCodewords
  let numShort := numBlocks - raw % numBlocks
  let shortLen := raw / numBlocks
  let div := ReedSolomon.divisor eccLen
  let bytes := ByteArray.mk data.toArray
  let mut blocks : Array ByteArray := #[]
  let mut k := 0
  for i in [0:numBlocks] do
    let len := shortLen - eccLen + (if i < numShort then 0 else 1)
    let dat := bytes.extract k (k + len)
    k := k + len
    let ecc := ReedSolomon.remainder dat div
    -- short blocks get a placeholder byte so all blocks line up; it is skipped below
    let dat := if i < numShort then dat.push 0 else dat
    blocks := blocks.push (ecc.foldl (·.push ·.val) dat)
  let mut out := Array.emptyWithCapacity raw
  for i in [0:shortLen + 1] do
    for j in [0:numBlocks] do
      if i != shortLen - eccLen || j ≥ numShort then
        out := out.push blocks[j]![i]!
  return Vector.ofFn fun i => out[i.val]!

/-- The modules codeword bits go in, in order: the two-column zigzag from the
bottom right, skipping function modules and the vertical timing column. -/
def codewordOrder (isFunction : Grid n) : Array (Nat × Nat) := Id.run do
  let mut out := #[]
  for k in [0:(n - 1) / 2] do
    let right := n - 1 - 2 * k
    let right := if right ≤ 6 then right - 1 else right
    let upward := (right + 1) &&& 2 == 0
    for vert in [0:n] do
      for j in [0:2] do
        let x := right - j
        let y := if upward then n - 1 - vert else vert
        if !isFunction.get x y then
          out := out.push (x, y)
  return out

namespace Canvas

/-- Places the codewords in `codewordOrder`; modules past the last bit stay light. -/
def drawCodewords (c : Canvas n) (data : ByteArray) : Canvas n := Id.run do
  let mut m := c.modules
  for ((x, y), i) in (codewordOrder c.isFunction).zipIdx do
    if i < data.size * 8 then
      m := m.set x y (data[i / 8]!.toNat.testBit (7 - i % 8))
  return { c with modules := m }

/-- XORs mask `m` into the non-function modules; applying it twice undoes it. -/
def applyMask (c : Canvas n) (m : Mask) : Canvas n :=
  { c with modules := .ofFn fun x y => c.modules.get x y ^^ (m.inverts x y && !c.isFunction.get x y) }

end Canvas

/-! The penalty rule, as qrcodegen scores it (ISO/IEC 18004 section 7.8.3). -/

namespace Penalty

def n1 := 3
def n2 := 3
def n3 := 40
def n4 := 10

/-- The last seven run lengths along a line, newest first. -/
abbrev History := Vector Nat 7

/-- Pushes a run length; the first run of a line gets the light border added. -/
def push (size : Nat) (h : History) (run : Nat) : History :=
  let run := if h[0] == 0 then run + size else run
  Vector.ofFn fun i => if h' : i.val = 0 then run else h[i.val - 1]'(by omega)

/-- Finder-like patterns, dark:light ratio 1:1:3:1:1 with 4 light on one side,
ending at the newest light run. -/
def countPatterns (h : History) : Nat :=
  let n := h[1]
  let core := n > 0 && h[2] == n && h[3] == n * 3 && h[4] == n && h[5] == n
  (if core && h[0] ≥ n * 4 && h[6] ≥ n then 1 else 0) +
    (if core && h[6] ≥ n * 4 && h[0] ≥ n then 1 else 0)

def terminateAndCount (size : Nat) (color : Bool) (run : Nat) (h : History) : Nat :=
  let (h, run) := if color then (push size h run, 0) else (h, run)
  countPatterns (push size h (run + size))

/-- Penalties N1 (runs of five or more) and N3 (finder-like patterns) along one
line of `size` modules. -/
def line (size : Nat) (get : Nat → Bool) : Nat := Id.run do
  let mut result := 0
  let mut color := false
  let mut run := 0
  let mut h : History := Vector.replicate 7 0
  for i in [0:size] do
    if get i == color then
      run := run + 1
      if run == 5 then result := result + n1
      else if run > 5 then result := result + 1
    else
      h := push size h run
      if !color then result := result + countPatterns h * n3
      color := get i
      run := 1
  return result + terminateAndCount size color run h * n3

end Penalty

/-- The penalty score of a masked symbol; lower is better. -/
def penaltyScore (g : Grid n) : Nat := Id.run do
  let mut result := 0
  for y in [0:n] do
    result := result + Penalty.line n fun x => g.get x y
  for x in [0:n] do
    result := result + Penalty.line n fun y => g.get x y
  for y in [0:n - 1] do
    for x in [0:n - 1] do
      let c := g.get x y
      if c == g.get (x + 1) y && c == g.get x (y + 1) && c == g.get (x + 1) (y + 1) then
        result := result + Penalty.n2
  -- N4: 10 points per 5 % the dark share strays from 50 %
  let dark := g.countDark
  let total := n * n
  let diff := if dark * 20 ≥ total * 10 then dark * 20 - total * 10 else total * 10 - dark * 20
  return result + ((diff + total - 1) / total - 1) * Penalty.n4

end Qrcode
