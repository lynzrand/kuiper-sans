/// Implementation of the `glyf`, `loca` and `maxp` tables, which contains glyph data.
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

type MaxZonesKind =
  | DoesNotUseTwilightZone = 1us
  | UsesTwilightZone = 2us

type MaxProfileTable = {
  n_glyphs: uint16
  max_points: uint16
  max_countours: uint16
  max_composite_points: uint16
  max_composite_countours: uint16
  max_zones: MaxZonesKind
  max_twilight_points: uint16
  max_storage: uint16
  max_func_defs: uint16
  max_instruction_defs: uint16
  max_stack_elements: uint16
  max_size_of_instructions: uint16
  max_component_elements: uint16
  max_component_depth: uint16
}

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
let write_glyf (glyf: GlyphTable) (w: BinaryWriter) : LocationTable =
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

let write_loca (loca: LocationTable) (w: BinaryWriter) =
  for offset in loca.offsets do
    write_u32_be w offset

type private CompositeData = {
  n_points: int
  n_countours: int
  depth: int
}

let private merge_composite_data (data: CompositeData array) =
  let n_points = data |> Array.sumBy (fun d -> d.n_points)
  let n_countours = data |> Array.sumBy (fun d -> d.n_countours)
  let depth = data |> Array.maxBy (fun d -> d.depth) |> (fun d -> d.depth + 1)
  {
    n_points = n_points
    n_countours = n_countours
    depth = depth
  }

let rec private calc_maxp_for_composite (glyf: GlyphTable) (data: CompositeGlyphData array) =
  data
  |> Array.map (fun d ->
    let glyph = glyf.contents[int d.glyphIndex]
    match glyph.data with
    | Simple simple -> {
        n_points = simple.x_coordinates.Length
        n_countours = simple.contour_end_point_indices.Length
        depth = 1
      }
    | Composite composite ->
      let sub_data = calc_maxp_for_composite glyf composite
      merge_composite_data sub_data
  )

let generate_maxp (glyf: GlyphTable) : MaxProfileTable =
  let mutable max_points = 0us
  let mutable max_countours = 0us
  let mutable max_composite_points = 0us
  let mutable max_composite_countours = 0us
  let mutable max_twilight_points = 0us
  let mutable max_storage = 0us
  let mutable max_func_defs = 0us
  let mutable max_instruction_defs = 0us
  let mutable max_stack_elements = 0us
  let mutable max_size_of_instructions = 0us
  let mutable max_component_elements = 0us
  let mutable max_component_depth = 0us

  for glyph in glyf.contents do
    match glyph.data with
    | Simple simple ->
      max_points <- max max_points (uint16 simple.x_coordinates.Length)
      max_countours <- max max_countours (uint16 simple.contour_end_point_indices.Length)
      max_size_of_instructions <- max max_size_of_instructions (uint16 simple.instructions.Length)
    | Composite composite ->
      let data = merge_composite_data (calc_maxp_for_composite glyf composite)
      max_composite_points <- max max_composite_points (uint16 data.n_points)
      max_composite_countours <- max max_composite_countours (uint16 data.n_countours)
      max_component_depth <- max max_component_depth (uint16 data.depth)

  {
    n_glyphs = uint16 glyf.contents.Length
    max_points = max_points
    max_countours = max_countours
    max_composite_points = max_composite_points
    max_composite_countours = max_composite_countours
    max_zones = MaxZonesKind.UsesTwilightZone
    max_twilight_points = max_twilight_points
    max_storage = max_storage
    max_func_defs = max_func_defs
    max_instruction_defs = max_instruction_defs
    max_stack_elements = max_stack_elements
    max_size_of_instructions = max_size_of_instructions
    max_component_elements = max_component_elements
    max_component_depth = max_component_depth
  }

let write_maxp (maxp: MaxProfileTable) (w: BinaryWriter) =
  write_u32_be w 0x00010000u
  write_u16_be w maxp.n_glyphs
  write_u16_be w maxp.max_points
  write_u16_be w maxp.max_countours
  write_u16_be w maxp.max_composite_points
  write_u16_be w maxp.max_composite_countours
  write_u16_be w (uint16 maxp.max_zones)
  write_u16_be w maxp.max_twilight_points
  write_u16_be w maxp.max_storage
  write_u16_be w maxp.max_func_defs
  write_u16_be w maxp.max_instruction_defs
  write_u16_be w maxp.max_stack_elements
  write_u16_be w maxp.max_size_of_instructions
  write_u16_be w maxp.max_component_elements
  write_u16_be w maxp.max_component_depth
