/-!
8-bit images, row-major: the grayscale input, and the bitmap the binarizer makes of
it, 1 for dark and 0 for light. Pixel `(x, y)` covers `[x, x + 1) × [y, y + 1)` of
the plane the finder and the perspective transform work in.
-/

namespace Qrcode.Decode

structure Image where
  width : Nat
  height : Nat
  pixels : ByteArray
  size_eq : pixels.size = width * height

namespace Image

instance : Inhabited Image := ⟨⟨0, 0, .empty, rfl⟩⟩

theorem index_lt {w h x y : Nat} (hx : x < w) (hy : y < h) : y * w + x < w * h :=
  calc y * w + x < y * w + w := by omega
    _ = (y + 1) * w := (Nat.succ_mul y w).symm
    _ ≤ h * w := Nat.mul_le_mul_right _ hy
    _ = w * h := Nat.mul_comm h w

/-- `pixels` as a `width` by `height` image, if it has that many bytes. -/
def ofBytes? (width height : Nat) (pixels : ByteArray) : Option Image :=
  if h : pixels.size = width * height then some ⟨width, height, pixels, h⟩ else none

/-- `acc` with `f k`, ..., `f (n - 1)` pushed. Specialized at each call, so the pixel
function is inlined rather than called through a closure. -/
@[specialize] def pushFn (f : Nat → UInt8) (n k : Nat) (acc : ByteArray) : ByteArray :=
  if k < n then pushFn f n (k + 1) (acc.push (f k)) else acc
termination_by n - k

theorem size_pushFn {f : Nat → UInt8} {n k : Nat} {acc : ByteArray} :
    (pushFn f n k acc).size = acc.size + (n - k) := by
  fun_induction pushFn f n k acc <;> simp_all [ByteArray.size_push] <;> omega

/-- The image whose pixel `(x, y)` is `f x y`. -/
@[inline] def ofFn (width height : Nat) (f : Nat → Nat → UInt8) : Image :=
  ⟨width, height, pushFn (fun i => f (i % width) (i / width)) (width * height) 0 (.emptyWithCapacity (width * height)),
    by rw [size_pushFn]; simp [ByteArray.emptyWithCapacity, ByteArray.size]; rfl⟩

/-- The pixel at `(x, y)`; 0 outside the image. -/
@[inline] def get (img : Image) (x y : Nat) : UInt8 :=
  if h : x < img.width ∧ y < img.height then
    img.pixels[y * img.width + x]'(img.size_eq ▸ index_lt h.1 h.2)
  else 0

/-- Whether bitmap pixel `(x, y)` is dark; outside the image is light. -/
@[inline] def dark (bm : Image) (x y : Int) : Bool :=
  x ≥ 0 && y ≥ 0 && bm.get x.toNat y.toNat != 0

/-- Whether the bitmap is dark at point `(x, y)` of the plane. -/
@[inline] def darkAt (bm : Image) (x y : Float) : Bool :=
  x ≥ 0 && y ≥ 0 && bm.get x.toUInt64.toNat y.toUInt64.toNat != 0

end Image

end Qrcode.Decode
