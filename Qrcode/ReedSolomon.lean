import Qrcode.GF256

/-!
Reed-Solomon parity as QR Codes use it: the remainder of the data polynomial,
times x^d, divided by the generator (x - 2^0)(x - 2^1)...(x - 2^(d-1)).
Ported from qrcodegen's `reedSolomonComputeDivisor` and `reedSolomonComputeRemainder`.
-/

namespace Qrcode.ReedSolomon

/-- The generator polynomial of degree `d`, highest degree first, without its
leading coefficient 1. -/
def divisor (d : Nat) : Vector GF256 d := Id.run do
  -- start with the monomial x^0
  let mut r : Vector GF256 d := Vector.ofFn fun i => if i.val + 1 = d then 1 else 0
  let mut root : GF256 := 1
  for _ in [0:d] do
    -- multiply by (x - root)
    r := Vector.ofFn fun j => r[j] * root + (if h : j.val + 1 < d then r[j.val + 1] else 0)
    root := root * 2
  return r

/-- The `d` parity bytes of `data`, given `divisor d`. -/
def remainder (data : ByteArray) (div : Vector GF256 d) : Vector GF256 d :=
  data.foldl (init := Vector.replicate d 0) fun r b =>
    let factor : GF256 := ⟨b⟩ + (if h : 0 < d then r[0] else 0)
    Vector.ofFn fun i => (if h : i.val + 1 < d then r[i.val + 1] else 0) + div[i] * factor

end Qrcode.ReedSolomon
