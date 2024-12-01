module rec Rynco.KuiperSans.Util.Math

/// A fixed-point number with 2 integer bits and 14 fractional bits.
[<Struct>]
type f2dot14 = {
  v: int16
} with

  member this.Value = this.v

  // Implicit conversion operators from float and to int16
  static member op_Implicit(f: f2dot14) : int16 = f.v
  static member op_Implicit(f: float) = float_to_f2dot14 f

[<Struct>]
type fixed_t = {
  v: int32
} with

  member this.Value = this.v

  // Implicit conversion operators from float and to int32
  static member op_Implicit(f: fixed_t) : int32 = f.v
  static member op_Implicit(f: float) = float_to_fixed f

/// Construct a `f2dot14` from an `int16`.
let f2dot14 (v: int16) : f2dot14 = { v = v }

/// Construct a `fixed_t` from an `int32`.
let fixed_t (v: int32) : fixed_t = { v = v }

/// Convert a float to a 2.14 fixed-point number.
let float_to_f2dot14 (f: float) : f2dot14 =
  let integer_part = int (floor f)
  let fractional_part = f - float integer_part
  let fractional_part = fractional_part * 16384.0
  let fractional_part = int fractional_part
  let v = int16 ((integer_part <<< 14) ||| fractional_part)
  f2dot14 v

/// Convert a float to a 16.16 fixed-point number.
let float_to_fixed (f: float) : fixed_t =
  let v = int32 (f * 65536.0)
  fixed_t v

let mk_version16dot16 major minor =
  let major = major &&& 0xFFFF
  let minor = minor &&& 0xFFFF
  (major <<< 16) ||| minor

type fword = int16
type ufword = uint16
