module Rynco.KuiperSans.Program

/// Convert a float to a 2.14 fixed-point number.
let float_to_fixed (f: float) : int16 =
  let integer_part = int f
  let fractional_part = f - float integer_part
  let fractional_part = fractional_part * 16384.0
  let fractional_part = int fractional_part
  int16 ((integer_part <<< 14) ||| fractional_part)

let mk_version16dot16 major minor =
  let major = major &&& 0xFFFF
  let minor = minor &&& 0xFFFF
  (major <<< 16) ||| minor

type table_dir = {
  version: uint32
  n_tables: uint16
  search_range: uint16
  entry_selector: uint16
  range_shift: uint16
}

type table_record = {
  tag: uint32
  checksum: uint32
  offset: uint32
  length: uint32
}

let calc_table_checksum (table: uint8 System.Memory) : uint32 =
  if table.Length % 4 <> 0 then
    failwith "Table buffer must be padded to a multiple of 4 bytes (although the on-disc format is not padded)"

  let u32_len = table.Length / 4
  let u32_tbl =
    System.Runtime.InteropServices.MemoryMarshal.Cast<uint8, uint32>(table.Span)
  let mutable sum = 0u
  for i in 0 .. u32_len - 1 do
    sum <- sum + u32_tbl.[i]

  sum

[<System.Flags>]
type header_flags =
  | BaselineY0 = 0x0001us
  | LeftSidebearingX0 = 0x0002us
  | InstructionsDependOnPointSize = 0x0004us
  | ForcePpemToInt = 0x0008us
  | InstructionsAlterAdvanceWidth = 0x0010us
  | LosslessFontData = 0x0800us
  | FontConverted = 0x1000us
  | OptimizedForClearType = 0x2000us
  | LastResortFont = 0x4000us

type index_to_loc_format =
  | Short = 0us
  | Long = 1us

type header_table = {
  major_version: uint16
  minor_version: uint16
  font_revision: uint32
  checksum_adjustment: uint32
  magic_number: uint32
  flags: header_flags
  units_per_em: uint16
  created: uint64
  modified: uint64
  x_min: int16
  y_min: int16
  x_max: int16
  y_max: int16
  mac_style: uint16
  lowest_rec_ppem: uint16
  font_direction_hint: int16
  index_to_loc_format: index_to_loc_format
  glyph_data_format: int16
}
