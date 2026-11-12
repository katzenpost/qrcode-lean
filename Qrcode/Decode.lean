import Qrcode.Decode.Binarize
import Qrcode.Decode.Sample
import Qrcode.Decode.Symbol

/-!
The decoder's entry points: a grayscale image in, the first symbol's payload out.
Every step is a pure function; the camera is the caller's business.
-/

namespace Qrcode.Decode

/-- Decodes `g`, or else its mirror image, as a camera that mirrors its frames shows a
symbol; errors are `g`'s own. -/
def decodeEither (v : Version) (g : Grid v.size) : Except String Decoded :=
  match decodeGrid v g with
  | .ok d => .ok d
  | .error e => (decodeGrid v (.ofFn fun x y => g.get y x)).mapError fun _ => e

/-- The first symbol found in bitmap `bm`: find finder triples, and for each, try the
likely versions; versions 7 and up are resampled at the size their version information
gives. -/
def decodeBitmap (bm : Image) : Except String Decoded := do
  let ts := triples (findFinders bm)
  if ts.isEmpty then throw "no finder patterns found"
  let mut err := ""
  for (bl, tl, tr) in ts do
    for v in versionGuesses bl tl tr (moduleSize bl tl tr) do
      let al := alignmentFor bm bl tl tr (moduleSize bl tl tr) v
      -- noise can hide the alignment pattern and fake one nearby, so the parallelogram's corner is tried too
      for a in if al.isSome then [al, none] else [none] do
        let g := sample bm (transform bl tl tr v.size a) v.size
        let v' := if v.val < 7 then v else (decodeVersion (versionWords g)).getD v
        match if v' == v then decodeEither v g else decodeEither v' (sampleSymbol bm bl tl tr v') with
        | .ok d => return d
        | .error e => err := s!"version {v'}: {e}"
  throw s!"no symbol decoded; last try {err}"

/-- The first symbol found in grayscale image `img`; if none is, the image is smoothed
and searched again, for noisy frames. -/
def decodeImage (img : Image) : Except String Decoded :=
  match decodeBitmap (binarize img) with
  | .ok d => .ok d
  | .error e => (decodeBitmap (binarize (smooth img))).mapError fun _ => e

/-- The payload of the first QR Code symbol in an 8-bit grayscale image, row-major,
`pixels.size = width * height`. -/
def decodeGray (width height : Nat) (pixels : ByteArray) : Except String ByteArray := do
  let some img := Image.ofBytes? width height pixels
    | throw s!"{pixels.size} bytes for a {width}x{height} image"
  return (← decodeImage img).payload

end Qrcode.Decode
