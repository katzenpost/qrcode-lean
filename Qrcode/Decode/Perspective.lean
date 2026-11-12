/-!
Plane projective transforms (homographies), as Heckbert derives them in
"Fundamentals of Texture Mapping and Image Warping" (1989): the unit square to any
quadrilateral, and through it any quadrilateral to any other.
-/

namespace Qrcode.Decode

/-- `(x, y) ↦ ((a x + b y + c) / w, (d x + e y + f) / w)` with `w = g x + h y + i`. -/
structure Homography where
  a : Float
  b : Float
  c : Float
  d : Float
  e : Float
  f : Float
  g : Float
  h : Float
  i : Float

namespace Homography

def apply (t : Homography) (x y : Float) : Float × Float :=
  let w := t.g * x + t.h * y + t.i
  ((t.a * x + t.b * y + t.c) / w, (t.d * x + t.e * y + t.f) / w)

/-- The transform taking the unit square's corners (0, 0), (1, 0), (1, 1), (0, 1) to
`p0`, `p1`, `p2`, `p3`. -/
def squareToQuad (p0 p1 p2 p3 : Float × Float) : Homography :=
  let ((x0, y0), (x1, y1), (x2, y2), (x3, y3)) := (p0, p1, p2, p3)
  let (sx, sy) := (x0 - x1 + x2 - x3, y0 - y1 + y2 - y3)
  let (dx1, dx2, dy1, dy2) := (x1 - x2, x3 - x2, y1 - y2, y3 - y2)
  let den := dx1 * dy2 - dx2 * dy1
  let g := (sx * dy2 - dx2 * sy) / den
  let h := (dx1 * sy - sx * dy1) / den
  ⟨x1 - x0 + g * x1, x3 - x0 + h * x3, x0, y1 - y0 + g * y1, y3 - y0 + h * y3, y0, g, h, 1⟩

/-- The adjugate matrix: the inverse up to a scale factor, which the transform ignores. -/
def adjugate (t : Homography) : Homography :=
  ⟨t.e * t.i - t.f * t.h, t.c * t.h - t.b * t.i, t.b * t.f - t.c * t.e,
   t.f * t.g - t.d * t.i, t.a * t.i - t.c * t.g, t.c * t.d - t.a * t.f,
   t.d * t.h - t.e * t.g, t.b * t.g - t.a * t.h, t.a * t.e - t.b * t.d⟩

/-- `s` after `t`. -/
def comp (s t : Homography) : Homography :=
  ⟨s.a * t.a + s.b * t.d + s.c * t.g, s.a * t.b + s.b * t.e + s.c * t.h, s.a * t.c + s.b * t.f + s.c * t.i,
   s.d * t.a + s.e * t.d + s.f * t.g, s.d * t.b + s.e * t.e + s.f * t.h, s.d * t.c + s.e * t.f + s.f * t.i,
   s.g * t.a + s.h * t.d + s.i * t.g, s.g * t.b + s.h * t.e + s.i * t.h, s.g * t.c + s.h * t.f + s.i * t.i⟩

/-- The transform taking each corner of quadrilateral `src` to the same corner of `dst`. -/
def quadToQuad (src dst : (Float × Float) × (Float × Float) × (Float × Float) × (Float × Float)) : Homography :=
  let (p0, p1, p2, p3) := src
  let (q0, q1, q2, q3) := dst
  (squareToQuad q0 q1 q2 q3).comp (squareToQuad p0 p1 p2 p3).adjugate

end Homography

end Qrcode.Decode
