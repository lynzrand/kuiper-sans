/// Implementation of the OpenType `hhea` table.
///
/// See: https://learn.microsoft.com/zh-cn/typography/opentype/spec/hhea
/// See: https://learn.microsoft.com/zh-cn/typography/opentype/spec/hhea
module Rynco.KuiperSans.Model.HorizontalHeader

open Rynco.KuiperSans.Util.Writer
open Rynco.KuiperSans.Util.Math

let major_version = 1us
let minor_version = 0us

type HorizontalHeader = {
  ascender: fword
  descender: fword
  line_gap: fword
  advance_width_max: ufword
  min_left_side_bearing: fword
  min_right_side_bearing: fword
  x_max_extent: fword
  caret_slope_rise: int16
  caret_slope_run: int16
  caret_offset: fword

  metric_data_format: int16
  number_of_hmetrics: uint16
}

let write_hhea_table (hhea: HorizontalHeader) (w: BinaryWriter) =
  write_u16_be w major_version
  write_u16_be w minor_version
  write_i16_be w hhea.ascender
  write_i16_be w hhea.descender
  write_i16_be w hhea.line_gap
  write_u16_be w hhea.advance_width_max
  write_i16_be w hhea.min_left_side_bearing
  write_i16_be w hhea.min_right_side_bearing
  write_i16_be w hhea.x_max_extent
  write_i16_be w hhea.caret_slope_rise
  write_i16_be w hhea.caret_slope_run
  write_i16_be w hhea.caret_offset

  // Reserved fields
  write_i16_be w 0s
  write_i16_be w 0s
  write_i16_be w 0s
  write_i16_be w 0s

  write_i16_be w hhea.metric_data_format
  write_u16_be w hhea.number_of_hmetrics
