/-!
GF(2^8) with the reducing polynomial x^8 + x^4 + x^3 + x^2 + 1 (0x11d), the field
of QR Code error correction. qrcodegen multiplies bit by bit; log and exp tables
give the same products faster.
-/

namespace Qrcode

/-- An element of GF(2^8). -/
structure GF256 where
  val : UInt8
  deriving DecidableEq, Inhabited, Repr

namespace GF256

/-- `(exp, log)` tables over the generator 2; `exp` is doubled to 512 entries so
products need no reduction modulo 255. -/
def tables : ByteArray × ByteArray := Id.run do
  let mut exp := ByteArray.mk (Array.replicate 512 0)
  let mut log := ByteArray.mk (Array.replicate 256 0)
  let mut x : Nat := 1
  for i in [0:255] do
    exp := exp.set! i x.toUInt8
    log := log.set! x i.toUInt8
    x := x <<< 1
    if x &&& 0x100 != 0 then
      x := x ^^^ 0x11d
  for i in [255:512] do
    exp := exp.set! i exp[i - 255]!
  return (exp, log)

def expTable : ByteArray := tables.1
def logTable : ByteArray := tables.2

instance : OfNat GF256 n := ⟨⟨n.toUInt8⟩⟩

@[inline] def exp (i : Nat) : GF256 := ⟨expTable[i]!⟩
@[inline] def log (x : GF256) : Nat := logTable[x.val.toNat]!.toNat

/-- Addition is XOR. -/
instance : Add GF256 := ⟨fun x y => ⟨x.val ^^^ y.val⟩⟩

def mul (x y : GF256) : GF256 :=
  if x.val == 0 || y.val == 0 then 0 else exp (log x + log y)

instance : Mul GF256 := ⟨mul⟩

end GF256

end Qrcode
