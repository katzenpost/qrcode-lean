import QrcodeTests.Components
import QrcodeTests.Symbols

/-! `lake test`: each encoder step against qrcodegen's, whole symbols, the voucher
codes, and the renderer. -/

open QrcodeTests

def main : IO UInt32 := do
  let failures ← IO.mkRef 0
  let run (name : String) (t : T Unit) : IO Unit := do
    IO.println name
    let t0 ← IO.monoMsNow
    t.run failures
    IO.println s!"{name}: {(← IO.monoMsNow) - t0} ms"
  run "unit" unit
  run "GF(2^8) and Reed-Solomon" reedSolomon
  run "segments" segments
  run "capacity" capacity
  run "codeword interleaving" codewords
  run "function patterns, format and version bits" functionPatternVectors
  run "masks and penalties" maskVectors
  run "symbols" symbolVectors
  run "voucher codes" voucherVectors
  let n ← failures.get
  if n == 0 then
    IO.println "all tests passed"
    return 0
  IO.eprintln s!"{n} checks failed"
  return 1
