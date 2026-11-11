/-!
Versions, error correction levels and the capacity tables of QR Code Model 2
(ISO/IEC 18004), as qrcodegen tabulates them.
-/

namespace Qrcode

/-- A version number, 1 to 40. Version `v` has `4v + 17` modules on a side. -/
structure Version where
  val : Nat
  one_le : 1 ≤ val
  le_forty : val ≤ 40
  deriving DecidableEq, Repr

namespace Version

def min : Version := ⟨1, by decide, by decide⟩
def max : Version := ⟨40, by decide, by decide⟩

def ofNat? (n : Nat) : Option Version :=
  if h : 1 ≤ n ∧ n ≤ 40 then some ⟨n, h.1, h.2⟩ else none

/-- Versions `lo` to `hi`, in order. -/
def range (lo hi : Version) : List Version :=
  (List.range (hi.val + 1 - lo.val)).filterMap fun i => ofNat? (lo.val + i)

def all : List Version := range min max

instance : LE Version := ⟨fun a b => a.val ≤ b.val⟩
instance : DecidableRel (α := Version) (· ≤ ·) := fun a b => Nat.decLe a.val b.val
instance : ToString Version := ⟨fun v => toString v.val⟩

/-- Modules on a side. -/
def size (v : Version) : Nat := v.val * 4 + 17

/-- Index into the 40-entry tables below. -/
@[inline] def idx (v : Version) : Fin 40 := ⟨v.val - 1, by have := v.le_forty; omega⟩

/-- Data and parity modules, after the function patterns and format and version
information are drawn; it includes the remainder bits. -/
def rawDataModules (v : Version) : Nat :=
  let ver := v.val
  let r := (16 * ver + 128) * ver + 64
  if ver < 2 then r else
    let numAlign := ver / 7 + 2
    let r := r - ((25 * numAlign - 10) * numAlign - 55)
    if ver < 7 then r else r - 36

/-- Codewords in the symbol, data and parity together. -/
def rawCodewords (v : Version) : Nat := v.rawDataModules / 8

/-- Centres of the alignment patterns along each axis, ascending. -/
def alignmentPositions (v : Version) : List Nat :=
  if v.val == 1 then [] else
    let numAlign := v.val / 7 + 2
    let step := if v.val == 32 then 26 else (v.val * 4 + numAlign * 2 + 1) / (numAlign * 2 - 2) * 2
    6 :: ((List.range (numAlign - 1)).map fun i => v.size - 7 - i * step).reverse

end Version

/-- The error correction level, from about 7 % to 30 % of codewords recoverable. -/
inductive Ecc where
  | low | medium | quartile | high
  deriving DecidableEq, Repr, Inhabited

namespace Ecc

def ordinal : Ecc → Fin 4
  | low => 0 | medium => 1 | quartile => 2 | high => 3

/-- The two bits the format information carries. -/
def formatBits : Ecc → Nat
  | low => 1 | medium => 0 | quartile => 3 | high => 2

instance : ToString Ecc := ⟨fun | low => "L" | medium => "M" | quartile => "Q" | high => "H"⟩

def all : List Ecc := [low, medium, quartile, high]

end Ecc

/-- Parity codewords per block, by level and version. -/
def eccCodewordsPerBlock : Vector (Vector Nat 40) 4 := #v[
  #v[ 7, 10, 15, 20, 26, 18, 20, 24, 30, 18, 20, 24, 26, 30, 22, 24, 28, 30, 28, 28, 28, 28, 30, 30, 26, 28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30],
  #v[10, 16, 26, 18, 24, 16, 18, 22, 22, 26, 30, 22, 22, 24, 24, 28, 28, 26, 26, 26, 26, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28],
  #v[13, 22, 18, 26, 18, 24, 18, 22, 20, 24, 28, 26, 24, 20, 30, 24, 28, 28, 26, 30, 28, 30, 30, 30, 30, 28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30],
  #v[17, 28, 22, 16, 22, 28, 26, 26, 24, 28, 24, 28, 22, 24, 24, 30, 28, 28, 26, 28, 30, 24, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30]]

/-- Error correction blocks, by level and version. -/
def numErrorCorrectionBlocks : Vector (Vector Nat 40) 4 := #v[
  #v[1, 1, 1, 1, 1, 2, 2, 2, 2, 4,  4,  4,  4,  4,  6,  6,  6,  6,  7,  8,  8,  9,  9, 10, 12, 12, 12, 13, 14, 15, 16, 17, 18, 19, 19, 20, 21, 22, 24, 25],
  #v[1, 1, 1, 2, 2, 4, 4, 4, 5, 5,  5,  8,  9,  9, 10, 10, 11, 13, 14, 16, 17, 17, 18, 20, 21, 23, 25, 26, 28, 29, 31, 33, 35, 37, 38, 40, 43, 45, 47, 49],
  #v[1, 1, 2, 2, 4, 4, 6, 6, 8, 8,  8, 10, 12, 16, 12, 17, 16, 18, 21, 20, 23, 23, 25, 27, 29, 34, 34, 35, 38, 40, 43, 45, 48, 51, 53, 56, 59, 62, 65, 68],
  #v[1, 1, 2, 4, 4, 4, 5, 6, 8, 8, 11, 11, 16, 16, 18, 16, 19, 21, 25, 25, 25, 34, 30, 32, 35, 37, 40, 42, 45, 48, 51, 54, 57, 60, 63, 66, 70, 74, 77, 81]]

/-- Parity codewords per block at level `e` in version `v`. -/
def eccPerBlock (v : Version) (e : Ecc) : Nat := eccCodewordsPerBlock[e.ordinal][v.idx]

/-- Error correction blocks at level `e` in version `v`. -/
def numBlocks (v : Version) (e : Ecc) : Nat := numErrorCorrectionBlocks[e.ordinal][v.idx]

/-- Data codewords a symbol of version `v` at level `e` holds. -/
def numDataCodewords (v : Version) (e : Ecc) : Nat :=
  v.rawCodewords - eccPerBlock v e * numBlocks v e

end Qrcode
