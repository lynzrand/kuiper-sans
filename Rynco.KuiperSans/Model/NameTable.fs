/// Implementation of the OpenType `name` table, which contains human-readable
/// names for the font.
///
/// See: https://learn.microsoft.com/zh-cn/typography/opentype/spec/name
module Rynco.KuiperSans.Model.NameTable

open Rynco.KuiperSans.Util.IO


/// The version of the name table. Always 1.
let private name_table_version = 1us

/// The platform ID. Always 0 (Unicode platform)
let private platform_id = 0us

/// The encoding ID. Always 4 (Unicode 2.0+ full repertoire)
let private encoding_id = 4us

type NameId =
  | Copyright = 0us
  | FontFamilyName = 1us
  | FontSubfamilyName = 2us
  | UniqueFontIdentifier = 3us
  | FullFontName = 4us
  | Version = 5us
  | PostscriptName = 6us
  | Trademark = 7us
  | Manufacturer = 8us
  | Designer = 9us
  | Description = 10us
  | VendorURL = 11us
  | DesignerURL = 12us
  | LicenseDescription = 13us
  | LicenseInfoURL = 14us
  | Reserved = 15us
  | PreferredFamily = 16us
  | PreferredSubfamily = 17us
  | CompatibleFull = 18us
  | SampleText = 19us
  | PostscriptCIDFindfontName = 20us
  | WWSFamilyName = 21us
  | WWSSubfamilyName = 22us
  | LightBackgroundPalette = 23us
  | DarkBackgroundPalette = 24us
  | VariationsPostscriptNamePrefix = 25us


type NameRecord = {
  /// The language in BCP 47 format (e.g. "en-US")
  lang: string
  name_id: NameId

  // Written as (len, string_start_offset), both uint16
  value: string
}

/// The actual user-facing name table structure
type NameTable = { records: NameRecord list }

/// An encoded name record, with the language ID gathered from existing records information
type private EncodedNameRecord = {
  lang_id: uint16
  name_id: uint16
  value: string
}

let write_name (name_table: NameTable) (w: BinaryWriter) =
  let name_acc = mk_string_accumulator System.Text.Encoding.BigEndianUnicode // UTF-16BE

  // Collect all used languages to make lang tag records
  let lang_tags =
    name_table.records
    |> List.distinctBy (fun r -> r.lang)
    |> List.map (fun r -> r.lang)
  let rev_lang_tags = lang_tags |> List.mapi (fun i lang -> lang, i) |> Map.ofList

  // Encode name record using their languages
  let encoded_records =
    name_table.records
    |> List.map (fun r ->
      let lang_id = rev_lang_tags.[r.lang]
      {
        lang_id = uint16 lang_id
        name_id = uint16 r.name_id
        value = r.value
      }
    )
    |> List.sortBy (fun r -> r.lang_id, r.name_id)

  // Will need to first calculate the size of the static table items, so we can
  // determine where to write the strings.
  let size_of_each_lang_tag = 2 (* string len *) + 2 (* string start offset *)

  let size_of_each_name_record =
    2 (* platform ID *)
    + 2 (* encoding ID *)
    + 2 (* lang ID *)
    + 2 (* name ID *)
    + 2 (* string len *)
    + 2 (* string start offset *)

  let name_table_rest_fields_size =
    2 (* version *)
    + 2 (* start of strings *)
    + 2 (* number of lang tags *)
    + 2 (* number of name records *)

  let name_table_size =
    name_table_rest_fields_size
    + (lang_tags.Length * size_of_each_lang_tag)
    + (encoded_records.Length * size_of_each_name_record)

  let strings_start = name_table_size

  // Write table header
  write_u16_be w name_table_version
  write_u16_be w (uint16 name_table.records.Length)
  write_u16_be w (uint16 strings_start)

  // Name records
  for record in encoded_records do
    write_u16_be w platform_id
    write_u16_be w encoding_id
    write_u16_be w record.lang_id
    write_u16_be w (uint16 record.name_id)

    let offset, len = add_string name_acc record.value
    write_u16_be w (uint16 len)
    write_u16_be w (uint16 offset)

  // Lang tag records
  write_u16_be w (uint16 lang_tags.Length)
  for record in lang_tags do
    let offset, len = add_string name_acc record
    write_u16_be w (uint16 len)
    write_u16_be w (uint16 offset)

  // Write the strings
  let strings = get_string_buf name_acc
  w.Write(strings)
