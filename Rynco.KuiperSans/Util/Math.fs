module Rynco.KuiperSans.Util.Math

type f2dot14 = int16

/// Convert a float to a 2.14 fixed-point number.
let float_to_fixed (f: float) : f2dot14 =
  let integer_part = int (floor f)
  let fractional_part = f - float integer_part
  let fractional_part = fractional_part * 16384.0
  let fractional_part = int fractional_part
  int16 ((integer_part <<< 14) ||| fractional_part)

let mk_version16dot16 major minor =
  let major = major &&& 0xFFFF
  let minor = minor &&& 0xFFFF
  (major <<< 16) ||| minor

type fword = int16
type ufword = uint16
