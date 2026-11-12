# The decoder

`Qrcode.Decode.decodeGray width height pixels` takes an 8-bit grayscale image,
row-major, and returns the payload of the first QR Code symbol it finds. The
steps follow ZXing's detector and are each a module under `Qrcode/Decode/`:

1. `Binarize`: a pixel is dark when it is darker than the mean of a square
   window around it, a side of the window an eighth of the image's larger side.
   A summed-area table gives each window's sum in four lookups (Bradley and
   Roth, 2007, without their bias towards light).
2. `Finder`: each row is scanned for dark, light, dark, light and dark runs in
   the ratio 1:1:3:1:1; a hit is confirmed down its column, along its row and
   along the diagonal. The last two checks allow each run one pixel more, as a
   binarized edge often lands a pixel off, a third of a module three pixels
   wide. Triples of candidates are ranked by how close they come
   to a right isosceles triangle, and ordered by a cross product.
3. `Sample`: the module size and the distances between finders give the
   version. Near where the finders put it, the bottom right alignment pattern
   is looked for, and the three finder centres and the alignment pattern fix a
   perspective transform (Heckbert, 1989, in `Perspective`). If that grid
   does not decode, the corner completing the finders' parallelogram is used
   instead, as noise can hide the alignment pattern and fake one nearby. Each
   module is read at its centre. From version 7 the version information decides the grid
   size.
4. `Format`: the nearest of the 32 format words, and of the 34 version words,
   to the bits read, corrects up to three wrong bits.
5. `Codewords`: unmasking, the codeword zigzag the encoder also uses,
   splitting the interleaved blocks, and Reed-Solomon correction of each block
   (Berlekamp-Massey, Chien search, Forney; in `ReedSolomon`).
6. `Payload`: numeric, alphanumeric, byte and kanji segments; kanji come back
   in Shift JIS, and ECI designators are read and dropped.

A grid that does not decode is tried again mirrored, for cameras that mirror
their frames. A frame in which no symbol decodes is smoothed with a 3 by 3
kernel and searched again, which recovers noisy frames but smears modules
under three pixels wide.

## Speed

`lake test` times `decodeGray` on a 640 by 480 frame holding the voucher symbol
(version 4) at 8 pixels a module, and on the same frame without it, which costs
both passes. On a 4-core x86-64 machine under load the times are about 10 ms
and 26 ms. Busy frames cost more: about 31 ms for a frame of random 3 by 4
pixel blocks, like a page of small print, and about 100 ms for white noise.

Of 30 renderings of the `noisy` test image, each with fresh noise, 28 decode.

## Limits

- One perspective transform per symbol, from three finders and one alignment
  pattern. Large versions bent by a lens may misread modules far from those
  points; reading every alignment pattern would correct that.
- Dark symbols on a light ground only; light on dark is not looked for.
- Structured append and FNC1 segments are not supported.
- Only the first symbol that decodes is returned.
