import Qrcode.Decode.Image

/-!
Binarization by a local mean: a pixel is dark when it is darker than the mean of a
square window around it, the method of Bradley and Roth ("Adaptive Thresholding Using
the Integral Image", 2007) without their bias towards light, as QR Codes are about half
dark. A summed-area table gives each window's sum in four lookups. Comparing with a
local mean copes with the uneven light of a camera frame, where one global threshold
would not.
-/

namespace Qrcode.Decode

/-- The bitmap of `img`, 1 for dark: darker than the mean of the window around the
pixel, clipped to the image. The window spans an eighth of the larger side, wider than
a finder's dark centre in a symbol that fills the frame. -/
def binarize (img : Image) : Image :=
  let (w, h, p) := (img.width, img.height, img.pixels)
  let r := max 4 (max w h / 16)
  -- sat[y * (w + 1) + x] is the sum of the pixels above and left of (x, y)
  let sat : Array Nat := Id.run do
    let mut s := Array.replicate ((w + 1) * (h + 1)) 0
    for y in [0:h] do
      let mut row := 0
      for x in [0:w] do
        row := row + p[y * w + x]!.toNat
        s := s.set! ((y + 1) * (w + 1) + x + 1) (s[y * (w + 1) + x + 1]! + row)
    return s
  Image.ofFn w h fun x y =>
    let (x0, x1) := (x - r, min w (x + r + 1))
    let (y0, y1) := (y - r, min h (y + r + 1))
    let sum := sat[y1 * (w + 1) + x1]! + sat[y0 * (w + 1) + x0]! - sat[y0 * (w + 1) + x1]! - sat[y1 * (w + 1) + x0]!
    if p[y * w + x]!.toNat * ((x1 - x0) * (y1 - y0)) < sum then 1 else 0

/-- `img` blurred by the 3 by 3 kernel [1 2 1] ⊗ [1 2 1] / 16, edges repeated: it evens
out sensor noise that would break up the runs the finder measures, at the cost of
smearing modules under three pixels wide. -/
def smooth (img : Image) : Image :=
  let (w, h, p) := (img.width, img.height, img.pixels)
  Image.ofFn w h fun x y =>
    -- rows above, at and below, and columns left and right, clamped to the image
    let (u, c, d) := ((y - 1) * w, y * w, min (h - 1) (y + 1) * w)
    let (l, r) := (x - 1, min (w - 1) (x + 1))
    let v := 4 * p[c + x]!.toNat + 2 * (p[c + l]!.toNat + p[c + r]!.toNat + p[u + x]!.toNat + p[d + x]!.toNat) +
      p[u + l]!.toNat + p[u + r]!.toNat + p[d + l]!.toNat + p[d + r]!.toNat
    (v / 16).toUInt8

end Qrcode.Decode
