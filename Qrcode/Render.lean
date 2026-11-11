import Qrcode.Encode

/-!
Text renderings for terminals: two modules per character cell, stacked, with
Unicode half blocks.
-/

namespace Qrcode.QrCode

/-- The symbol as rows of half blocks with a quiet zone of `border` modules.
With `inkDark` the glyphs' ink marks the dark modules, for dark text on a light
background; pass `false` for light text on a dark background, so the ink marks
the light modules and the symbol still reads dark on light. -/
def toHalfBlocks (q : QrCode) (border : Nat := 4) (inkDark : Bool := true) : List String :=
  let b : Int := border
  let span := q.size + 2 * border
  let ink (x y : Nat) : Bool := q.module (x - b) (y - b) == inkDark
  (List.range ((span + 1) / 2)).map fun r =>
    String.ofList <| (List.range span).map fun x =>
      -- an odd span leaves the last row's lower half outside, in the quiet zone
      let lower := if 2 * r + 1 < span then ink x (2 * r + 1) else !inkDark
      match ink x (2 * r), lower with
      | true, true => '█' | true, false => '▀' | false, true => '▄' | false, false => ' '

end Qrcode.QrCode
