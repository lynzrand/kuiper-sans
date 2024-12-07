/// Implementation of the `cmap` table.

module Rynco.KuiperSans.Model.CharacterMapping

open Rynco.KuiperSans.Util.Writer

let cmap_version = 0us
let platform_id = 0us // Unicode
let encoding_id = 4us // Unicode 2.0 and onwards semantics, full repertoire

type SequentialMapGroup = {
  start_code: uint32
  end_code: uint32
  start_glyph_id: uint32
}

type Format12Table = {
  // format: uint16 = 12
  // reserved: uint16 = 0
  // byte_length_including_header: uint32
  language: uint32
  // count: uint32
  groups: SequentialMapGroup list
}

let write_format_12_table (writer: System.IO.BinaryWriter) (table: Format12Table) =
  let size_per_entry = 3 * 4
  let header_size = 2 + 2 + 4 + 4 + 4
  let size = header_size + table.groups.Length * size_per_entry

  write_u16_be writer 12us
  write_u16_be writer 0us
  write_u32_be writer (uint32 size)
  write_u32_be writer table.language
  write_u32_be writer (uint32 table.groups.Length)

  for group in table.groups do
    write_u32_be writer group.start_code
    write_u32_be writer group.end_code
    write_u32_be writer group.start_glyph_id

type CmapSubtable = Format12 of Format12Table

type EncodingRecord = {
  platform_id: uint16
  encoding_id: uint16
  // offset: uint32
  table: CmapSubtable
}

type CharacterMappingTable = {
  // version: uint16 = 0
  // n_tables: uint16
  tables: EncodingRecord list
}

let write_cmap (writer: System.IO.BinaryWriter) (table: CharacterMappingTable) =
  write_u16_be writer 0us
  write_u16_be writer (uint16 table.tables.Length)

  let header_size = 4
  let record_size = 2 + 2 + 4

  // Write dummy records to skip to the actual tables
  let start_of_records = writer.BaseStream.Position
  for _ in table.tables do
    write_u16_be writer platform_id
    write_u16_be writer encoding_id
    write_u32_be writer (uint32 0)

  List.iteri
    (fun i record ->
      let start_of_table = writer.BaseStream.Position
      match record.table with
      | Format12 table -> write_format_12_table writer table
      let end_of_table = writer.BaseStream.Position
      let table_start_offset = start_of_table - start_of_records + int64 header_size

      // Write the actual offset
      let record_offset = int start_of_records + i * record_size
      writer.Seek(record_offset + 4, System.IO.SeekOrigin.Begin) |> ignore
      write_u32_be writer (uint32 table_start_offset)

      // position back to the end of the table
      writer.Seek(int end_of_table, System.IO.SeekOrigin.Begin) |> ignore
    )
    table.tables
