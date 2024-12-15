module Rynco.KuiperSans.Program

open Rynco.KuiperSans.Model

let head: Header.HeaderTable = {
  major_version = uint16 1
  minor_version = uint16 0
  font_revision = uint32 0
  checksum_adjustment = uint32 0
  magic_number = uint32 0x5F0F3CF5
  flags = Header.HeaderFlags.Nothing
  units_per_em = uint16 16384
  created = uint64 0
  modified = uint64 0
  mac_style = Header.MacStyle.Regular
  x_min = int16 0
  x_max = int16 16384
  y_min = int16 0
  y_max = int16 16384
  lowest_rec_ppem = uint16 10
  font_direction_hint = int16 2
  glyph_data_format = int16 0
}

// Map all characters to glyph 0
let cmap: CharacterMapping.CharacterMappingTable = {
  tables = [
    {
      platform_id = uint16 0
      encoding_id = uint16 4
      table =
        CharacterMapping.CmapSubtable.Format12 {
          language = uint32 0
          groups = [
            {
              start_code = uint32 0
              end_code = uint32 0x10FFFF
              start_glyph_id = uint32 0
            }
          ]
        }
    }
  ]
}

// Create a simple glyph with a single contour: a square from (0, 0) to (16384, 16384)
#nowarn "3391"
let only_glyph: GlyphData.GlyphData = {
  common = {
    x_min = int16 0
    y_min = int16 0
    x_max = int16 16384
    y_max = int16 16384
  }
  data =
    GlyphData.Simple {
      contour_end_point_indices = [| uint16 3 |]
      instructions = [||]
      flags = Array.replicate 4 (GlyphData.SimpleGlyphFlags.OnCurve)
      x_coordinates = [| int16 0; int16 16384; int16 16384; int16 0 |]
      y_coordinates = [| int16 0; int16 0; int16 16384; int16 16384 |]
    }
}
let glyf: GlyphData.GlyphTable = { contents = [| only_glyph |] }

let hhea: HorizontalHeader.HorizontalHeader = {
  ascender = int16 16384
  descender = int16 0
  line_gap = int16 0
  advance_width_max = uint16 16384
  min_left_side_bearing = int16 0
  min_right_side_bearing = int16 0
  x_max_extent = int16 16384
  caret_slope_rise = int16 1
  caret_slope_run = int16 0
  caret_offset = int16 0
  metric_data_format = int16 0
  number_of_hmetrics = uint16 1
}

let hmtx: HorizontalMetrics.HorizontalMetrics = {
  h_metrics = [|
    {
      advance_width = uint16 16384
      left_side_bearing = int16 0
    }
  |]
  left_side_bearings = [| int16 0 |]
}

let name: NameTable.NameTable = {
  records = [
    {
      lang = "en"
      name_id = NameTable.NameId.FullFontName
      value = "KuiperSans-Regular"
    }
    {
      lang = "en"
      name_id = NameTable.NameId.FontFamilyName
      value = "KuiperSans"
    }
    {
      lang = "en"
      name_id = NameTable.NameId.FontSubfamilyName
      value = "Regular"
    }
  ]
}

let post: PostScriptData.PostScriptData = {
  italic_angle = 0.0
  underline_position = int16 0
  underline_thickness = int16 0
  is_fixed_pitch = PostScriptData.IsFixedPitch.No
  min_mem_type42 = uint32 0
  max_mem_type42 = uint32 0
  min_mem_type1 = uint32 0
  max_mem_type1 = uint32 0
}

let os2: Os2Data.Os2Table = {
  version = uint16 4
  x_avg_char_width = int16 0
  us_weight_class = uint16 400

  usage_permission = Os2Data.UsagePermissionKind.EditableEmbedding
  fs_type = Os2Data.FsTypeUpper.Nothing

  y_subscript_x_size = int16 650
  y_subscript_y_size = int16 700
  y_subscript_x_offset = int16 0
  y_subscript_y_offset = int16 0
  y_superscript_x_size = int16 650
  y_superscript_y_size = int16 700
  y_superscript_x_offset = int16 0
  y_superscript_y_offset = int16 0
  y_strikeout_size = int16 50
  y_strikeout_position = int16 350

  s_family_class = Os2Data.SFamilyClass.NoClassification
  panose_classification = {
    bFamilyType = 0uy
    bSerifStyle = 0uy
    bWeight = 0uy
    bProportion = 0uy
    bContrast = 0uy
    bStrokeVariation = 0uy
    bArmStyle = 0uy
    bLetterform = 0uy
    bMidline = 0uy
    bXHeight = 0uy
  }

  unicode_range1 = uint32 0xffffffff
  unicode_range2 = uint32 0
  unicode_range3 = uint32 0
  unicode_range4 = uint32 0

  ach_vend_id = "    "
  fs_selection = Os2Data.FsSelectionKind.Regular

  first_char_index = uint16 0x0020
  last_char_index = uint16 0xffff

  s_typo_ascender = int16 16384
  s_typo_descender = int16 0
  s_typo_line_gap = int16 0
  us_win_ascent = uint16 16384
  us_win_descent = uint16 0

  ul_code_page_range1 = uint32 0xffffffff
  ul_code_page_range2 = uint32 0

  sx_height = int16 0
  s_cap_height = int16 0

  us_default_char = uint16 0
  us_break_char = uint16 0x0020
  us_max_context = uint16 0
}

let buf = FontFile.make_font head hhea hmtx name post glyf cmap os2

FontFile.verify_font_file buf
System.IO.File.WriteAllBytes("KuiperSans-Regular.ttf", buf)
