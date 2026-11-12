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

/-! Decoding: syndromes, the Berlekamp-Massey error locator, Chien search for its
roots and Forney's formula for the error values. Polynomials below are arrays,
lowest degree first. -/

namespace Qrcode.ReedSolomon

/-- `p(x)`, by Horner's rule. -/
def eval (p : Array GF256) (x : GF256) : GF256 := p.foldr (fun c acc => acc * x + c) 0

/-- The `d` syndromes of `block`, highest degree first: its values at 2^0 to 2^(d-1),
all zero for a code word. -/
def syndromes (block : ByteArray) (d : Nat) : Array GF256 :=
  (Array.range d).map fun i => block.foldl (fun acc b => acc * GF256.exp i + ⟨b⟩) 0

/-- `p - c x^k q`, which is `p + c x^k q` in characteristic 2. -/
def addScaledShift (p q : Array GF256) (c : GF256) (k : Nat) : Array GF256 := Id.run do
  let mut r := p ++ Array.replicate (q.size + k - min p.size (q.size + k)) 0
  for h : i in [0:q.size] do
    r := r.modify (i + k) (· + c * q[i])
  return r

/-- The shortest linear recurrence generating `s` (Berlekamp-Massey): the error
locator polynomial, and its length. -/
def berlekampMassey (s : Array GF256) : Array GF256 × Nat := Id.run do
  let mut c : Array GF256 := #[1]
  let mut b : Array GF256 := #[1]
  let mut l := 0
  let mut m := 1
  let mut bd : GF256 := 1
  for h : n in [0:s.size] do
    let d := (List.range l).foldl (fun d i => d + c.getD (i + 1) 0 * s.getD (n - (i + 1)) 0) s[n]
    if d == 0 then
      m := m + 1
    else if 2 * l ≤ n then
      let t := c
      c := addScaledShift c b (d / bd) m
      l := n + 1 - l
      b := t
      bd := d
      m := 1
    else
      c := addScaledShift c b (d / bd) m
      m := m + 1
  return (c, l)

/-- Corrects up to `d / 2` wrong bytes in `block`, data then `d` parity bytes; `none`
when the errors are beyond that and detected. -/
def correct (block : ByteArray) (d : Nat) : Option ByteArray := do
  let s := syndromes block d
  if s.all (· == 0) then return block
  let (sigma, l) := berlekampMassey s
  guard (2 * l ≤ d)
  -- the error evaluator S(x) sigma(x) mod x^d, and the formal derivative of sigma
  let omega := (Array.range d).map fun k =>
    (List.range (k + 1)).foldl (fun acc i => acc + sigma.getD i 0 * s.getD (k - i) 0) 0
  let deriv := (Array.range (sigma.size - 1)).map fun i => if i % 2 == 0 then sigma[i + 1]! else 0
  let n := block.size
  let mut out := block
  let mut found := 0
  -- the byte at index n - 1 - j has locator 2^j; its error is a root of sigma(2^-j)
  for j in [0:n] do
    let xinv := GF256.exp ((255 - j % 255) % 255)
    if eval sigma xinv == 0 then
      let e := GF256.exp j * eval omega xinv / eval deriv xinv
      out := out.set! (n - 1 - j) (out[n - 1 - j]! ^^^ e.val)
      found := found + 1
  guard (found == l && (syndromes out d).all (· == 0))
  return out

end Qrcode.ReedSolomon
