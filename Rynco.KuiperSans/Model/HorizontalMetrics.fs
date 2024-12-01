/// Implementation of the `hmtx` table, which contains horizontal metrics for each glyph.
///
/// See: https://learn.microsoft.com/zh-cn/typography/opentype/spec/hmtx
module Rynco.KuiperSans.Model.HorizontalMetrics

open Rynco.KuiperSans.Util.Math
open Rynco.KuiperSans.Util.Writer

type LongHorizontalMetric = {
  advance_width: ufword
  left_side_bearing: fword
}

type HorizontalMetrics = {
  h_metrics: LongHorizontalMetric[]
  left_side_bearings: fword[]
}

let verify_hmat (hhea: HorizontalHeader.HorizontalHeader) (hmat: HorizontalMetrics) =
  if hmat.h_metrics.Length <> int hhea.number_of_hmetrics then
    failwithf
      "The number of hmetrics in the hmtx table (%d) does not match the number of hmetrics in the hhea table (%d)"
      hmat.h_metrics.Length
      hhea.number_of_hmetrics

// TODO: verify that len(left_side_bearings) == num_glyphs - len(h_metrics)

let write_hmat (hmat: HorizontalMetrics) (w: BinaryWriter) =
  for i = 0 to hmat.h_metrics.Length - 1 do
    let m = hmat.h_metrics.[i]
    write_u16_be w m.advance_width
    write_i16_be w m.left_side_bearing

  for i = 0 to hmat.left_side_bearings.Length - 1 do
    write_i16_be w hmat.left_side_bearings.[i]
