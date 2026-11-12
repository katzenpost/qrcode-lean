import Qrcode.Grid
import Qrcode.Version
import Qrcode.Decode.Finder
import Qrcode.Decode.Perspective

/-!
From three finders to a module grid, after ZXing's `Detector`: estimate the module
size and the version, look for the bottom right alignment pattern, and sample each
module's centre through the perspective transform these four points fix.
-/

namespace Qrcode.Decode

/-- The module size along the symbol's own axes. A finder's size is measured along the
image's rows and columns, which in a symbol turned by angle θ is 1 / max(|cos θ|, |sin θ|)
times too long; the top edge gives θ. -/
def moduleSize (bl tl tr : Finder) : Float :=
  let (dx, dy) := (tr.x - tl.x, tr.y - tl.y)
  (bl.size + tl.size + tr.size) / 3 * max dx.abs dy.abs / (dx * dx + dy * dy).sqrt

/-- Versions to try: the one the distances between finder centres suggest, then its
neighbours. -/
def versionGuesses (bl tl tr : Finder) (m : Float) : List Version :=
  let side := (dist tl tr + dist tl bl) / 2 / m + 7
  let v := ((side - 17) / 4).round.toUInt64.toNat
  [v, v + 1, v - 1].filterMap Version.ofNat?

/-- Whether runs dark, light, dark, light, dark could cross an alignment pattern's
centre: the middle three, a module each, in the ratio 1:1:1, and the outer dark ring
at least half a module, as it may run on into dark modules beside it. -/
def alignmentRuns (r : Array Nat) : Bool :=
  let mid := r.extract 1 4
  let t := mid.foldl (· + ·) 0
  ratio #[1, 1, 1] 2 mid && 6 * r[0]! ≥ t && 6 * r[4]! ≥ t

/-- The centre of the alignment pattern nearest `(ex, ey)`, within `r` of it: a dark
module ringed by light, ringed by dark, as `alignmentRuns` sees it along its row and
again down its column. -/
def findAlignment (bm : Image) (ex ey r : Float) : Option (Float × Float) := Id.run do
  let lo (c : Float) := (c - r).toUInt64.toNat  -- negative saturates to 0
  let mut best : Option (Float × Float × Float) := none
  for y in [lo ey:min bm.height (ey + r).toUInt64.toNat] do
    for (h, cx) in runWindows (rowRuns bm y (lo ex) (min bm.width (ex + r).toUInt64.toNat)) (lo ex) alignmentRuns do
      let t := h[1]! + h[2]! + h[3]!
      let (v, oy) := crossRuns bm cx.toUInt64.toNat y 0 1 (2 * t)
      if alignmentRuns v && 5 * diff (v[1]! + v[2]! + v[3]!) t < 2 * t then
        let cy := y.toFloat + oy
        let d := (cx - ex) ^ 2 + (cy - ey) ^ 2
        if best.all (d < ·.2.2) then
          best := some (cx, cy, d)
  return best.map fun (x, y, _) => (x, y)

/-- The transform from module coordinates to the image for a symbol `n` modules wide:
finder centres at (3.5, 3.5), (n - 3.5, 3.5) and (3.5, n - 3.5), and the bottom right
alignment pattern's centre at (n - 6.5, n - 6.5) if found, else the corner completing
the parallelogram at (n - 3.5, n - 3.5). -/
def transform (bl tl tr : Finder) (n : Nat) (align : Option (Float × Float)) : Homography :=
  let d := n.toFloat - 3.5
  let (src, dst) := match align with
    | some p => ((d - 3, d - 3), p)
    | none => ((d, d), (tr.x - tl.x + bl.x, tr.y - tl.y + bl.y))
  .quadToQuad ((3.5, 3.5), (d, 3.5), src, (3.5, d)) ((tl.x, tl.y), (tr.x, tr.y), dst, (bl.x, bl.y))

/-- The bottom right alignment pattern of a version `v` symbol, searched for around
where the finders put it, in a widening square. -/
def alignmentFor (bm : Image) (bl tl tr : Finder) (m : Float) (v : Version) : Option (Float × Float) :=
  if v.val == 1 then none else
    -- three modules in from the parallelogram's corner, towards the top left
    let k := 1 - 3 / (v.size.toFloat - 7)
    let (ex, ey) := (tl.x + k * (tr.x - tl.x + bl.x - tl.x), tl.y + k * (tr.y - tl.y + bl.y - tl.y))
    [4, 8, 16].findSome? fun a => findAlignment bm ex ey (a * m)

/-- The grid of an `n` by `n` symbol: each module's centre through `t`. -/
def sample (bm : Image) (t : Homography) (n : Nat) : Grid n :=
  Grid.ofFn fun x y =>
    let (px, py) := t.apply (x.toFloat + 0.5) (y.toFloat + 0.5)
    bm.darkAt px py

/-- The grid of a version `v` symbol whose finders are `bl`, `tl` and `tr`. -/
def sampleSymbol (bm : Image) (bl tl tr : Finder) (v : Version) : Grid v.size :=
  sample bm (transform bl tl tr v.size (alignmentFor bm bl tl tr (moduleSize bl tl tr) v)) v.size

end Qrcode.Decode
