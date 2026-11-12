import Qrcode.Decode.Image

/-!
Finding the three finder patterns in a bitmap, after ZXing's `FinderPatternFinder`.
Every line through a finder's centre crosses dark, light, dark, light and dark runs in
the ratio 1:1:3:1:1, whatever the symbol's rotation. Each row is scanned for that
ratio; a hit is confirmed down its column, again along its row through the corrected
centre, and along the diagonal, then averaged into the nearby hits before it.
-/

namespace Qrcode.Decode

/-- A finder pattern candidate: its centre, its module size along the image axes,
and how many scan lines confirmed it. -/
structure Finder where
  x : Float
  y : Float
  size : Float
  count : Nat
  deriving Inhabited, Repr

/-- Whether runs `r` are in the ratio `w`, each within `slack` / 4 of its share plus `px`
pixels: `|r_i - w_i t / W| < (slack / 4) w_i t / W + px`, for `t` and `W` the sums of `r`
and `w`. -/
def ratio (w : Array Nat) (slack : Nat) (r : Array Nat) (px : Nat := 0) : Bool :=
  let t := r.foldl (· + ·) 0
  let ww := w.foldl (· + ·) 0
  t ≥ ww && r.size == w.size && (r.zip w).all fun (ri, wi) =>
    4 * ww * ri < 4 * wi * t + slack * wi * t + 4 * ww * px && 4 * wi * t < 4 * ww * ri + slack * wi * t + 4 * ww * px

def finderRatio (slack : Nat) (r : Array Nat) (px : Nat := 0) : Bool := ratio #[1, 1, 3, 1, 1] slack r px

/-- Pixels of colour `dark` from `(x, y)` on along `(dx, dy)`, at most `limit`. -/
def runLength (bm : Image) (x y dx dy : Int) (dark : Bool) (limit : Nat) : Nat := Id.run do
  let mut n : Nat := 0
  for _ in [0:limit] do
    if bm.dark (x + n * dx) (y + n * dy) == dark then n := n + 1 else break
  return n

/-- The five runs through `(x, y)` along `(dx, dy)`, each at most `limit` long: the dark
run holding the pixel, and a light then a dark run on either side; and how far along
the direction the middle run's centre is from the pixel's corner `(x, y)`. -/
def crossRuns (bm : Image) (x y dx dy : Int) (limit : Nat) : Array Nat × Float :=
  let back := runLength bm x y (-dx) (-dy) true limit
  let fwd := runLength bm (x + dx) (y + dy) dx dy true limit
  let b := runLength bm (x - back * dx) (y - back * dy) (-dx) (-dy) false limit
  let a := runLength bm (x - (back + b) * dx) (y - (back + b) * dy) (-dx) (-dy) true limit
  let d := runLength bm (x + (fwd + 1) * dx) (y + (fwd + 1) * dy) dx dy false limit
  let e := runLength bm (x + (fwd + 1 + d) * dx) (y + (fwd + 1 + d) * dy) dx dy true limit
  (#[a, b, back + fwd, d, e], (fwd.toFloat - back.toFloat + 2) / 2)

/-- Run lengths along row `y` from column `x0` to `x1`, alternating colours, the first
light (and empty if the row starts dark). -/
def rowRuns (bm : Image) (y x0 x1 : Nat) : Array Nat := Id.run do
  let mut runs := #[]
  let mut color := false
  let mut len := 0
  for x in [x0:x1] do
    let d := bm.pixels[y * bm.width + x]! != 0
    if d == color then
      len := len + 1
    else
      runs := runs.push len
      color := d
      len := 1
  return runs.push len

/-- The windows of five runs starting with a dark one that satisfy `ok`: each window's
runs and the centre of its middle run, from column `x0`. -/
def runWindows (runs : Array Nat) (x0 : Nat) (ok : Array Nat → Bool) : Array (Array Nat × Float) := Id.run do
  let mut out := #[]
  let mut start := x0
  for h : k in [0:runs.size] do
    if k % 2 == 1 && k + 4 < runs.size then
      let r := runs.extract k (k + 5)
      if ok r then
        out := out.push (r, (start + r[0]! + r[1]!).toFloat + r[2]!.toFloat / 2)
    start := start + runs[k]
  return out

/-- `|a - b|` for naturals. -/
@[inline] def diff (a b : Nat) : Nat := max a b - min a b

/-- Confirms a row hit centred at `cx` on row `y` with runs `r`: down the column, then
along the row through the corrected centre, then along the diagonal. The last two allow
each run a pixel more, as a binarized edge often lands a pixel off, a third of a module
three pixels wide; the column check stays strict, as it turns away most false hits. -/
def confirm (bm : Image) (y : Nat) (r : Array Nat) (cx : Float) : Option Finder := do
  let t := r.foldl (· + ·) 0
  let x := cx.toUInt64.toNat
  let (v, oy) := crossRuns bm x y 0 1 t
  let tv := v.foldl (· + ·) 0
  guard (finderRatio 2 v && 5 * diff tv t < 2 * t)
  let cy := y.toFloat + oy
  let (hr, ox) := crossRuns bm x cy.toUInt64.toNat 1 0 t
  let th := hr.foldl (· + ·) 0
  guard (finderRatio 2 hr 1 && 5 * diff th t < 2 * t)
  let cx := x.toFloat + ox
  guard (finderRatio 3 (crossRuns bm cx.toUInt64.toNat cy.toUInt64.toNat 1 1 t).1 1)
  return ⟨cx, cy, (th + tv).toFloat / 14, 1⟩

/-- Adds a confirmed centre, averaged into an earlier candidate of about the same
place and size if there is one. -/
def addCenter (cs : Array Finder) (f : Finder) : Array Finder :=
  match cs.findIdx? fun c => (c.x - f.x).abs ≤ c.size && (c.y - f.y).abs ≤ c.size &&
      (c.size - f.size).abs ≤ max 1 c.size with
  | some i => cs.modify i fun c =>
    let n := c.count.toFloat
    ⟨(c.x * n + f.x) / (n + 1), (c.y * n + f.y) / (n + 1), (c.size * n + f.size) / (n + 1), c.count + 1⟩
  | none => cs.push f

/-- Finder pattern candidates in bitmap `bm`. -/
def findFinders (bm : Image) : Array Finder := Id.run do
  let mut cs := #[]
  for y in [0:bm.height] do
    for (r, cx) in runWindows (rowRuns bm y 0 bm.width) 0 (finderRatio 2) do
      if let some f := confirm bm y r cx then
        cs := addCenter cs f
  return cs

def dist (a b : Finder) : Float := ((a.x - b.x) ^ 2 + (a.y - b.y) ^ 2).sqrt

/-- Three finders as bottom left, top left and top right: the top left one is opposite
the longest side, and the cross product's sign tells the other two apart (image y
grows downwards). -/
def order (p q r : Finder) : Finder × Finder × Finder :=
  let (a, b, c) :=
    if dist q r ≥ dist p q && dist q r ≥ dist p r then (q, p, r)
    else if dist p r ≥ dist q r && dist p r ≥ dist p q then (p, q, r)
    else (p, r, q)
  if (c.x - b.x) * (a.y - b.y) - (c.y - b.y) * (a.x - b.x) < 0 then (c, b, a) else (a, b, c)

/-- Triples of candidates that may be one symbol's finders, ordered by `order`, best
first: of the dozen candidates most often confirmed, those whose module sizes are
within a factor of two (perspective shrinks the far ones), ranked by how far their
triangle is from right isosceles. -/
def triples (cs : Array Finder) : List (Finder × Finder × Finder) := Id.run do
  let cs := (cs.qsort fun a b => a.count > b.count).extract 0 12
  let mut scored : Array (Float × Finder × Finder × Finder) := #[]
  for i in [0:cs.size] do
    for j in [i + 1:cs.size] do
      for k in [j + 1:cs.size] do
        let (p, q, r) := (cs[i]!, cs[j]!, cs[k]!)
        let sizes := [p.size, q.size, r.size]
        if sizes.foldl max 0 ≤ 2 * sizes.foldl min p.size then
          let ds := [dist p q ^ 2, dist q r ^ 2, dist p r ^ 2]
          let (lo, hi) := (ds.foldl min (dist p q ^ 2), ds.foldl max 0)
          let mid := ds.foldl (· + ·) 0 - lo - hi
          -- a right isosceles triangle has hi = 2 lo = 2 mid
          let skew := ((hi - 2 * mid).abs + (hi - 2 * lo).abs) / hi
          if skew < 1 then
            scored := scored.push (skew, order p q r)
  return ((scored.qsort fun a b => a.1 < b.1).map (·.2)).toList.take 4

end Qrcode.Decode
