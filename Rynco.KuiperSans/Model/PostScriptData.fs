/// Implementation of the `post` table, which contains PostScript data.
///
/// See: https://learn.microsoft.com/zh-cn/typography/opentype/spec/post
module Rynco.KuiperSans.Model.PostScriptData

open Rynco.KuiperSans.Util.Math
open Rynco.KuiperSans.Util.Writer

let private version = 0x00030000ul // version 3.0
let private unknown_mem = 0u

type IsFixedPitch =
  | Yes = 1u
  | No = 0u

type PostScriptData = {
  italic_angle: fixed_t
  underline_position: fword
  underline_thickness: fword
  is_fixed_pitch: IsFixedPitch
  min_mem_type42: uint32
  max_mem_type42: uint32
  min_mem_type1: uint32
  max_mem_type1: uint32
}

let default_postscript_data = {
  italic_angle = float_to_fixed 0.0
  underline_position = 0s
  underline_thickness = 0s
  is_fixed_pitch = IsFixedPitch.No
  min_mem_type42 = unknown_mem
  max_mem_type42 = unknown_mem
  min_mem_type1 = unknown_mem
  max_mem_type1 = unknown_mem
}

let write_post_table (post: PostScriptData) (w: BinaryWriter) =
  write_u32_be w version
  write_i32_be w (int32 post.italic_angle)
  write_i16_be w post.underline_position
  write_i16_be w post.underline_thickness
  write_u32_be w (uint32 post.is_fixed_pitch)
  write_u32_be w post.min_mem_type42
  write_u32_be w post.max_mem_type42
  write_u32_be w post.min_mem_type1
  write_u32_be w post.max_mem_type1
