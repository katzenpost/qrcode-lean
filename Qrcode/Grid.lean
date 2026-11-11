/-!
A square grid of `n` by `n` modules, row-major, `true` for dark. Coordinates
outside the grid read as light and writes to them are dropped, as in qrcodegen's
`getModule`, so drawing near the edge needs no special cases.
-/

namespace Qrcode

structure Grid (n : Nat) where
  cells : Array Bool
  size_eq : cells.size = n * n

namespace Grid

theorem index_lt {n x y : Nat} (hx : x < n) (hy : y < n) : y * n + x < n * n :=
  Nat.lt_of_lt_of_le (Nat.add_lt_add_left hx _)
    (by rw [← Nat.succ_mul]; exact Nat.mul_le_mul_right _ hy)

/-- All light. -/
def light (n : Nat) : Grid n := ⟨Array.replicate (n * n) false, Array.size_replicate⟩

def ofFn (f : Nat → Nat → Bool) : Grid n :=
  ⟨Array.ofFn fun (i : Fin (n * n)) => f (i.val % n) (i.val / n), Array.size_ofFn⟩

@[inline] def get (g : Grid n) (x y : Nat) : Bool :=
  if h : x < n ∧ y < n then g.cells[y * n + x]'(g.size_eq ▸ index_lt h.1 h.2) else false

@[inline] def set (g : Grid n) (x y : Nat) (b : Bool) : Grid n :=
  if h : x < n ∧ y < n then
    ⟨g.cells.set (y * n + x) b (g.size_eq ▸ index_lt h.1 h.2), by simp [g.size_eq]⟩
  else g

def countDark (g : Grid n) : Nat := g.cells.foldl (fun c b => if b then c + 1 else c) 0

/-- Packed row-major, most significant bit first, the last byte padded with zeros. -/
def toBytes (g : Grid n) : ByteArray := Id.run do
  let mut out := ByteArray.mk (Array.replicate ((n * n + 7) / 8) 0)
  for i in [0:g.cells.size] do
    if g.cells[i]! then
      out := out.set! (i / 8) (out[i / 8]! ||| (0x80 >>> (i % 8).toUInt8))
  return out

end Grid

end Qrcode
