/// Implements the `OS/2` table.

module Rynco.KuiperSans.Model.Os2Data

open Rynco.KuiperSans.Util.Math
open Rynco.KuiperSans.Util.IO

type WeightClass =
  | Thin = 100us
  | ExtraLight = 200us
  | Light = 300us
  | Regular = 400us
  | Medium = 500us
  | SemiBold = 600us
  | Bold = 700us
  | ExtraBold = 800us
  | Black = 900us

type WidthClass =
  | UltraCondensed = 1us
  | ExtraCondensed = 2us
  | Condensed = 3us
  | SemiCondensed = 4us
  | Normal = 5us
  | SemiExpanded = 6us
  | Expanded = 7us
  | ExtraExpanded = 8us
  | UltraExpanded = 9us

type UsagePermissionKind =
  | InstallableEmbedding = 0us
  | RestrictedLicenseEmbedding = 2us
  | PreviewPrintEmbedding = 4us
  | EditableEmbedding = 8us

[<System.Flags>]
type FsTypeUpper =
  | Nothing = 0x0000us
  | NoSubsetting = 0x0100us
  | BitmapEmbeddingOnly = 0x0200us

type BareFamilyClass =
  | NoClassification = 0
  | OldStyleSerifs = 1
  | TransitionalSerifs = 2
  | ModernSerifs = 3
  | ClarendonSerifs = 4
  | SlabSerifs = 5
  | FreeformSerifs = 7
  | SansSerif = 8
  | Ornamentals = 9
  | Scripts = 10
  | Symbolic = 12

type OldStyleSerifKind =
  | NoClassification = 0
  | IBMRoundedLegibility = 1
  | Garalde = 2
  | Venetian = 3
  | ModifiedVenetian = 4
  | DutchModern = 5
  | DutchTraditional = 6
  | Contemporary = 7
  | Calligraphic = 8
  | Miscellaneous = 15

type TransitionalSerifKind =
  | NoClassification = 0
  | DirectLine = 1
  | Script = 2
  | Miscellaneous = 15

type ModernSerifKind =
  | NoClassification = 0
  | Italian = 1
  | Script = 2
  | Miscellaneous = 15

type ClarendonSerifKind =
  | NoClassification = 0
  | Clarendon = 1
  | Modern = 2
  | Traditional = 3
  | Newspaper = 4
  | StubSerif = 5
  | Monotone = 6
  | Typewriter = 7
  | Miscellaneous = 15

type SlabSerifKind =
  | NoClassification = 0
  | Monotone = 1
  | Humanist = 2
  | Geometric = 3
  | Swiss = 4
  | Typewriter = 5
  | Miscellaneous = 15

type FreeformSerifKind =
  | NoClassification = 0
  | Modern = 1
  | Miscellaneous = 15

type SansSerifKind =
  | NoClassification = 0
  | IBMNeoGrotesqueGothic = 1
  | Humanist = 2
  | LowXRoundGeometric = 3
  | HighXRoundGeometric = 4
  | NeoGrotesqueGothic = 5
  | ModifiedNeoGrotesqueGothic = 6
  | TypewriterGothic = 9
  | Matrix = 10
  | Miscellaneous = 15

type OrnamentalKind =
  | NoClassification = 0
  | Engraver = 1
  | BlackLetter = 2
  | Decorative = 3
  | ThreeDimensional = 4
  | Miscellaneous = 15

type ScriptKind =
  | NoClassification = 0
  | Uncial = 1
  | BrushJoined = 2
  | FormalJoined = 3
  | MonotoneJoined = 4
  | Calligraphic = 5
  | BrushUnjoined = 6
  | FormalUnjoined = 7
  | MonotoneUnjoined = 8
  | Miscellaneous = 15

type SymbolicKind =
  | NoClassification = 0
  | MixedSerif = 3
  | OldstyleSerif = 6
  | NeoGrotesqueSansSerif = 7
  | Miscellaneous = 15

type SFamilyClass =
  | NoClassification
  | OldStyleSerifs of OldStyleSerifKind
  | TransitionalSerifs of TransitionalSerifKind
  | ModernSerifs of ModernSerifKind
  | ClarendonSerifs of ClarendonSerifKind
  | SlabSerifs of SlabSerifKind
  | FreeformSerifs of FreeformSerifKind
  | SansSerif of SansSerifKind
  | Ornamentals of OrnamentalKind
  | Scripts of ScriptKind
  | Symbolic of SymbolicKind

let s_family_class_to_int (cls: SFamilyClass) : uint16 =
  match cls with
  | NoClassification -> 0us
  | OldStyleSerifs kind -> 1us <<< 8 ||| uint16 kind
  | TransitionalSerifs kind -> 2us <<< 8 ||| uint16 kind
  | ModernSerifs kind -> 3us <<< 8 ||| uint16 kind
  | ClarendonSerifs kind -> 4us <<< 8 ||| uint16 kind
  | SlabSerifs kind -> 5us <<< 8 ||| uint16 kind
  | FreeformSerifs kind -> 7us <<< 8 ||| uint16 kind
  | SansSerif kind -> 8us <<< 8 ||| uint16 kind
  | Ornamentals kind -> 9us <<< 8 ||| uint16 kind
  | Scripts kind -> 10us <<< 8 ||| uint16 kind
  | Symbolic kind -> 12us <<< 8 ||| uint16 kind

type PanroseClassification = {
  bFamilyType: byte
  bSerifStyle: byte
  bWeight: byte
  bProportion: byte
  bContrast: byte
  bStrokeVariation: byte
  bArmStyle: byte
  bLetterform: byte
  bMidline: byte
  bXHeight: byte
}

[<System.Flags>]
type FsSelectionKind =
  | Italic = 0x0001us
  | Underscore = 0x0002us
  | Negative = 0x0004us
  | Outlined = 0x0008us
  | Strikeout = 0x0010us
  | Bold = 0x0020us
  | Regular = 0x0040us
  | UseTypoMetrics = 0x0080us
  | WWS = 0x0100us
  | Oblique = 0x0200us

type Os2Table = {
  version: uint16 // = 4
  x_avg_char_width: fword
  us_weight_class: uint16

  // The following two fields should be bitwise ORed together forming a single 16-bit value
  usage_permission: UsagePermissionKind
  fs_type: FsTypeUpper

  y_subscript_x_size: fword
  y_subscript_y_size: fword
  y_subscript_x_offset: fword
  y_subscript_y_offset: fword
  y_superscript_x_size: fword
  y_superscript_y_size: fword
  y_superscript_x_offset: fword
  y_superscript_y_offset: fword
  y_strikeout_size: fword
  y_strikeout_position: fword

  // https://learn.microsoft.com/zh-cn/typography/opentype/spec/ibmfc
  s_family_class: SFamilyClass
  panose_classification: PanroseClassification

  // https://learn.microsoft.com/zh-cn/typography/opentype/spec/os2#ur
  unicode_range1: uint32
  unicode_range2: uint32
  unicode_range3: uint32
  unicode_range4: uint32

  ach_vend_id: string // set to 4 spaces
  fs_selection: FsSelectionKind

  first_char_index: uint16 // =0x0020
  last_char_index: uint16 // =0xffff

  s_typo_ascender: fword
  s_typo_descender: fword
  s_typo_line_gap: fword
  us_win_ascent: ufword
  us_win_descent: ufword

  ul_code_page_range1: uint32
  ul_code_page_range2: uint32

  sx_height: fword
  s_cap_height: fword

  us_default_char: uint16 // = 0
  us_break_char: uint16 // = 0x0020
  us_max_context: uint16 // max kerning/ligature range
}


let write_os2 (table: Os2Table) (writer: System.IO.BinaryWriter) =
  write_u16_be writer table.version
  write_fword writer table.x_avg_char_width
  write_u16_be writer table.us_weight_class
  write_u16_be writer (uint16 (uint16 table.usage_permission ||| uint16 table.fs_type))
  write_fword writer table.y_subscript_x_size
  write_fword writer table.y_subscript_y_size
  write_fword writer table.y_subscript_x_offset
  write_fword writer table.y_subscript_y_offset
  write_fword writer table.y_superscript_x_size
  write_fword writer table.y_superscript_y_size
  write_fword writer table.y_superscript_x_offset
  write_fword writer table.y_superscript_y_offset
  write_fword writer table.y_strikeout_size
  write_fword writer table.y_strikeout_position
  write_u16_be writer (s_family_class_to_int table.s_family_class)
  write_u8_be writer table.panose_classification.bFamilyType
  write_u8_be writer table.panose_classification.bSerifStyle
  write_u8_be writer table.panose_classification.bWeight
  write_u8_be writer table.panose_classification.bProportion
  write_u8_be writer table.panose_classification.bContrast
  write_u8_be writer table.panose_classification.bStrokeVariation
  write_u8_be writer table.panose_classification.bArmStyle
  write_u8_be writer table.panose_classification.bLetterform
  write_u8_be writer table.panose_classification.bMidline
  write_u8_be writer table.panose_classification.bXHeight
  write_u32_be writer table.unicode_range1
  write_u32_be writer table.unicode_range2
  write_u32_be writer table.unicode_range3
  write_u32_be writer table.unicode_range4
  write_u32_be writer (System.BitConverter.ToUInt32(System.Text.Encoding.ASCII.GetBytes(table.ach_vend_id), 0))
  write_u16_be writer (uint16 table.fs_selection)
  write_u16_be writer table.first_char_index
  write_u16_be writer table.last_char_index
  write_fword writer table.s_typo_ascender
  write_fword writer table.s_typo_descender
  write_fword writer table.s_typo_line_gap
  write_ufword writer table.us_win_ascent
  write_ufword writer table.us_win_descent
  write_u32_be writer table.ul_code_page_range1
  write_u32_be writer table.ul_code_page_range2
  write_fword writer table.sx_height
  write_fword writer table.s_cap_height
  write_u16_be writer table.us_default_char
  write_u16_be writer table.us_break_char
  write_u16_be writer table.us_max_context
