/// Implementation of the `glyf` and `loca` tables, which contains glyph data.
///
/// See: https://learn.microsoft.com/zh-cn/typography/opentype/spec/glyf
module Rynco.KuiperSans.Model.GlyphData

open System
open Rynco.KuiperSans.Util.Math

type CommonGlyphHeader = {
  x_min: int16
  y_min: int16
  x_max: int16
  y_max: int16
}

[<Flags>]
type SimpleGlyphFlags =
  | OnCurve = 0x01us
  | XShort = 0x02us // Don't use XShort
  | YShort = 0x04us // Don't use YShort
  | Repeat = 0x08us
  | XSame = 0x10us // if XShort
  | YSame = 0x20us // if YShort
  | XPositive = 0x10us // if !XShort
  | YPositive = 0x20us // if !YShort
  | OverlapSimple = 0x40us

type SimpleGlyphFlagsOrRepeat =
  | Flags of SimpleGlyphFlags
  | RepeatCount of uint8

type SimpleGlyphData = {
  contour_end_point_indices: uint16[]
  instructions: uint8[]
  flags: SimpleGlyphFlagsOrRepeat[]
  x_coordinates: int16[]
  y_coordinates: int16[]
}

[<Flags>]
type CompositeGlyphFlags =
  | Arg1And2AreWords = 0x0001us
  | ArgsAreXyValues = 0x0002us
  | RoundXyToGrid = 0x0004us
  | WeHaveAScale = 0x0008us
  | MoreComponents = 0x0020us
  | WeHaveAnXAndYScale = 0x0040us
  | WeHaveATwoByTwo = 0x0080us
  | WeHaveInstructions = 0x0100us
  | UseMyMetrics = 0x0200us
  | OverlapCompound = 0x0400us
  | ScaledComponentOffset = 0x0800us
  | UnscaledComponentOffset = 0x1000us

type CompositeXformData =
  | Scale of f2dot14
  | ScaleXY of f2dot14 * f2dot14
  | TwoByTwo of f2dot14 * f2dot14 * f2dot14 * f2dot14
  | NoData

type CompositeGlyphData = {
  flags: CompositeGlyphFlags
  glyphIndex: uint16
  argument1: int16 // x-offset for component or point number; type depends on bits 0 and 1 in component flags
  argument2: int16 // y-offset for component or point number; type depends on bits 0 and 1 in component flags
  xform: CompositeXformData
  instructions: uint8[]
}

type GlyphDataBody =
  | Simple of SimpleGlyphData
  | Composite of CompositeGlyphData[]

type GlyphData = {
  common: CommonGlyphHeader
  data: GlyphDataBody
}

/// The actual glyph table structure
type GlyphTable = { contents: GlyphData[] }

// Always using the long format for the `loca` table.
/// The actual location table structure. Should be generated from the `glyf` table during serialization.
type LocationTable = { offsets: uint32[] }

open Rynco.KuiperSans.Util.Writer

let private write_glyph (glyph: GlyphData) (w: BinaryWriter) =
  let write_glyph_common (common: CommonGlyphHeader) =
    write_i16_be w common.x_min
    write_i16_be w common.y_min
    write_i16_be w common.x_max
    write_i16_be w common.y_max

  match glyph.data with
  | Simple simple ->
    let n_countour = simple.contour_end_point_indices.Length

    write_u16_be w (uint16 n_countour)
    write_glyph_common glyph.common

    for x in simple.contour_end_point_indices do
      write_u16_be w x

    let n_instructions = simple.instructions.Length
    for b in simple.instructions do
      write_u8_be w b

    for f in simple.flags do
      match f with
      | Flags flags -> write_u8_be w (uint8 flags)
      | RepeatCount count -> write_u8_be w count

    for x in simple.x_coordinates do
      write_i16_be w x

    for y in simple.y_coordinates do
      write_i16_be w y

  | Composite composite ->
    let n_countour = -1s
    write_i16_be w n_countour

    write_glyph_common glyph.common

    for c in composite do
      write_u16_be w (uint16 c.flags)
      write_u16_be w c.glyphIndex
      write_i16_be w c.argument1
      write_i16_be w c.argument2

      match c.xform with
      | Scale s -> write_i16_be w (int16 s)
      | ScaleXY(x, y) ->
        write_i16_be w (int16 x)
        write_i16_be w (int16 y)
      | TwoByTwo(a, b, c, d) ->
        write_i16_be w (int16 a)
        write_i16_be w (int16 b)
        write_i16_be w (int16 c)
        write_i16_be w (int16 d)
      | NoData -> ()

      let n_instructions = c.instructions.Length
      if n_instructions <> 0 then
        write_u16_be w (uint16 n_instructions)
        for i in c.instructions do
          write_u8_be w i

/// Writes the glyph table to the binary writer. Also generates the location table to be written.
let write_glyph_table (glyf: GlyphTable) (w: BinaryWriter) : LocationTable =
  let start_pos = w.BaseStream.Position
  let mutable loca_offsets_rev = []

  for glyph in glyf.contents do
    let loca_offset = w.BaseStream.Position - start_pos
    loca_offsets_rev <- (uint32 loca_offset) :: loca_offsets_rev
    write_glyph glyph w

  let one_past_end = w.BaseStream.Position - start_pos
  loca_offsets_rev <- (uint32 one_past_end) :: loca_offsets_rev

  let loca_offsets = loca_offsets_rev |> List.rev |> List.toArray
  { offsets = loca_offsets }
