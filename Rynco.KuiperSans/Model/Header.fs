/// Implementation of the OpenType `head` table.
///
/// https://learn.microsoft.com/zh-cn/typography/opentype/spec/head
module Rynco.KuiperSans.Model.Header

open Rynco.KuiperSans.Util.Writer

[<System.Flags>]
type HeaderFlags =
  | BaselineY0 = 0x0001us
  | LeftSidebearingX0 = 0x0002us
  | InstructionsDependOnPointSize = 0x0004us
  | ForcePpemToInt = 0x0008us
  | InstructionsAlterAdvanceWidth = 0x0010us
  | LosslessFontData = 0x0800us
  | FontConverted = 0x1000us
  | OptimizedForClearType = 0x2000us
  | LastResortFont = 0x4000us

type IndexToLocFormat =
  | Short = 0us
  | Long = 1us

type HeaderTable = {
  major_version: uint16
  minor_version: uint16
  font_revision: uint32
  checksum_adjustment: uint32
  magic_number: uint32
  flags: HeaderFlags
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
  // index_to_loc_format: IndexToLocFormat
  glyph_data_format: int16
}

let write_head (hdr: HeaderTable) (w: BinaryWriter) =
  write_u16_be w hdr.major_version
  write_u16_be w hdr.minor_version
  write_u32_be w hdr.font_revision
  write_u32_be w hdr.checksum_adjustment // FIXME: This value needs to be written back after the table is written
  write_u32_be w hdr.magic_number
  write_u16_be w (uint16 hdr.flags)
  write_u16_be w hdr.units_per_em
  write_u64_be w hdr.created
  write_u64_be w hdr.modified
  write_i16_be w hdr.x_min
  write_i16_be w hdr.y_min
  write_i16_be w hdr.x_max
  write_i16_be w hdr.y_max
  write_u16_be w hdr.mac_style
  write_u16_be w hdr.lowest_rec_ppem
  write_i16_be w hdr.font_direction_hint
  // write_u16_be w (uint16 hdr.index_to_loc_format)
  write_u16_be w (uint16 IndexToLocFormat.Long)
  write_i16_be w hdr.glyph_data_format
