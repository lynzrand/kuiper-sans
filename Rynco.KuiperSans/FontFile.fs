module Rynco.KuiperSans.FontFile

open Util.IO

type TableDirectory = {
  version: uint32
  n_tables: uint16
  search_range: uint16
  entry_selector: uint16
  range_shift: uint16
}

type TableRecord = {
  tag: uint32
  checksum: uint32
  offset: uint32
  length: uint32
}

let calc_table_checksum (table: uint8 System.Memory) : uint32 =
  if table.Length % 4 <> 0 then
    failwith "Table buffer must be padded to a multiple of 4 bytes (although the on-disc format is not padded)"

  let u32_len = table.Length / 4
  let u32_tbl =
    System.Runtime.InteropServices.MemoryMarshal.Cast<uint8, uint32>(table.Span)
  let mutable sum = 0u
  for i in 0 .. u32_len - 1 do
    sum <- sum + (to_be_32 u32_tbl.[i])

  sum

let pad_to_4bytes (w: System.IO.BinaryWriter) =
  let padding = 4 - (int w.BaseStream.Position % 4)
  if padding <> 4 then
    for _ in 1..padding do
      w.Write(uint8 0)

type private IncompleteTableRecord = {
  tag: uint32
  checksum: uint32
  buffer: uint8 array
}

type private TableAllocator = {
  mutable tables: IncompleteTableRecord list // Reverse order
}

let calc_tag (name: string) =
  let tag = System.BitConverter.ToUInt32(System.Text.Encoding.ASCII.GetBytes(name), 0)
  to_be_32 tag

let get_tag_name (tag: uint32) =
  System.Text.Encoding.ASCII.GetString(System.BitConverter.GetBytes(to_be_32 tag))

let private add_table (name: string) (buffer: uint8 array) (alloc: TableAllocator) =
  let checksum = calc_table_checksum (new System.Memory<uint8>(buffer))
  let tag = calc_tag name
  let record = {
    tag = tag
    checksum = checksum
    buffer = buffer
  }
  alloc.tables <- record :: alloc.tables

let make_font
  (head: Model.Header.HeaderTable)
  (hhea: Model.HorizontalHeader.HorizontalHeader)
  (hmtx: Model.HorizontalMetrics.HorizontalMetrics)
  (name: Model.NameTable.NameTable)
  (post: Model.PostScriptData.PostScriptData)
  (glyf: Model.GlyphData.GlyphTable)
  (cmap: Model.CharacterMapping.CharacterMappingTable)
  (os2: Model.Os2Data.Os2Table)
  : uint8 array =
  let maxp = Model.GlyphData.generate_maxp glyf

  // Wow that's a lot of streams to write to
  let head_w = new System.IO.MemoryStream()
  let hhea_w = new System.IO.MemoryStream()
  let hmtx_w = new System.IO.MemoryStream()
  let name_w = new System.IO.MemoryStream()
  let post_w = new System.IO.MemoryStream()
  let glyf_w = new System.IO.MemoryStream()
  let loca_w = new System.IO.MemoryStream()
  let maxp_w = new System.IO.MemoryStream()
  let cmap_w = new System.IO.MemoryStream()
  let os2_w = new System.IO.MemoryStream()

  Model.Header.write_head head (new System.IO.BinaryWriter(head_w))
  Model.HorizontalHeader.write_hhea hhea (new System.IO.BinaryWriter(hhea_w))
  Model.HorizontalMetrics.write_hmtx hmtx (new System.IO.BinaryWriter(hmtx_w))
  Model.NameTable.write_name name (new System.IO.BinaryWriter(name_w))
  Model.PostScriptData.write_post post (new System.IO.BinaryWriter(post_w))
  let loca = Model.GlyphData.write_glyf glyf (new System.IO.BinaryWriter(glyf_w))
  Model.GlyphData.write_loca loca (new System.IO.BinaryWriter(loca_w))
  Model.GlyphData.write_maxp maxp (new System.IO.BinaryWriter(maxp_w))
  Model.CharacterMapping.write_cmap cmap (new System.IO.BinaryWriter(cmap_w))
  Model.Os2Data.write_os2 os2 (new System.IO.BinaryWriter(os2_w))

  // Pad all tables to 4 bytes
  pad_to_4bytes (new System.IO.BinaryWriter(head_w))
  pad_to_4bytes (new System.IO.BinaryWriter(hhea_w))
  pad_to_4bytes (new System.IO.BinaryWriter(hmtx_w))
  pad_to_4bytes (new System.IO.BinaryWriter(name_w))
  pad_to_4bytes (new System.IO.BinaryWriter(post_w))
  pad_to_4bytes (new System.IO.BinaryWriter(glyf_w))
  pad_to_4bytes (new System.IO.BinaryWriter(loca_w))
  pad_to_4bytes (new System.IO.BinaryWriter(maxp_w))
  pad_to_4bytes (new System.IO.BinaryWriter(cmap_w))
  pad_to_4bytes (new System.IO.BinaryWriter(os2_w))

  let alloc = { tables = [] }
  add_table "head" (head_w.GetBuffer()) alloc
  add_table "hhea" (hhea_w.GetBuffer()) alloc
  add_table "hmtx" (hmtx_w.GetBuffer()) alloc
  add_table "name" (name_w.GetBuffer()) alloc
  add_table "post" (post_w.GetBuffer()) alloc
  add_table "glyf" (glyf_w.GetBuffer()) alloc
  add_table "loca" (loca_w.GetBuffer()) alloc
  add_table "maxp" (maxp_w.GetBuffer()) alloc
  add_table "cmap" (cmap_w.GetBuffer()) alloc
  add_table "OS/2" (os2_w.GetBuffer()) alloc

  let tables = List.rev alloc.tables
  let n_tables = tables.Length

  let final_stream = new System.IO.MemoryStream()
  let final_writer = new System.IO.BinaryWriter(final_stream)

  let search_range =
    (1 <<< (int (System.Math.Floor(System.Math.Log(float n_tables, 2.0)))) * 16)
  let entry_selector = int (System.Math.Log(float search_range / 16.0, 2.0))
  let range_shift = (n_tables * 16) - search_range

  // File header and table directory
  write_u32_be final_writer 0x00010000u // version
  write_u16_be final_writer (uint16 n_tables)
  write_u16_be final_writer (uint16 search_range)
  write_u16_be final_writer (uint16 entry_selector)
  write_u16_be final_writer (uint16 range_shift)

  // Lay out tables
  let sizeof_table_record = 16 // 4 x 32-bit fields
  let header_size = int final_stream.Position
  let tables_start = header_size + (n_tables * sizeof_table_record)

  let offsets, _ =
    (List.mapFold
      (fun offset record ->
        let len = record.buffer.Length
        offset, offset + len
      )
      tables_start
      tables)

  List.iter2
    (fun record offset ->
      write_u32_be final_writer record.tag
      write_u32_be final_writer record.checksum
      write_u32_be final_writer (uint32 offset)
      write_u32_be final_writer (uint32 record.buffer.Length)
    )
    tables
    offsets

  List.iter (fun record -> final_writer.Write(record.buffer)) tables

  // TODO: write the final checksum offset in the head table
  let head_tag = calc_tag "head"
  let head_table_offset =
    (List.fold2 (fun acc record offset -> if record.tag = head_tag then offset else acc) 0 tables offsets)

  let buffer = final_stream.GetBuffer()
  let checksum = calc_table_checksum (new System.Memory<uint8>(buffer))
  // To compute: set it to 0, sum the entire font as uint32, then store 0xB1B0AFBA - sum. If the font is used as a component in a font collection file, the value of this field will be invalidated by changes to the file structure and font table directory, and must be ignored.
  let checksum_adjustment = 0xB1B0AFBAu - checksum
  let adjusted_head = {
    head with
        checksum_adjustment = checksum_adjustment
  }
  final_stream.Seek(head_table_offset, System.IO.SeekOrigin.Begin) |> ignore
  Model.Header.write_head adjusted_head final_writer

  final_stream.GetBuffer()

let read_file_header (r: System.IO.BinaryReader) : TableDirectory = {
  version = read_u32_be r
  n_tables = read_u16_be r
  search_range = read_u16_be r
  entry_selector = read_u16_be r
  range_shift = read_u16_be r
}

let read_table_record (r: System.IO.BinaryReader) : TableRecord = {
  tag = read_u32_be r
  checksum = read_u32_be r
  offset = read_u32_be r
  length = read_u32_be r
}

let verify_font_file (font: uint8 array) =
  let r = new System.IO.BinaryReader(new System.IO.MemoryStream(font))
  let header = read_file_header r
  let records = Array.init (int header.n_tables) (fun _ -> read_table_record r)

  // Verify checksums
  for record in records do
    r.BaseStream.Seek(int64 record.offset, System.IO.SeekOrigin.Begin) |> ignore
    let buffer = r.ReadBytes(int record.length)
    let checksum = calc_table_checksum (new System.Memory<uint8>(buffer))
    let tbl_name = get_tag_name record.tag
    if checksum <> record.checksum && tbl_name <> "head" then
      failwithf "Checksum mismatch for table %s" (get_tag_name record.tag)

  // Verify checksum adjustment in head table
  // head.checksum_adjustment = 0xB1B0AFBA - sum(font)
  // => sum(font) + head.checksum_adjustment = 0xB1B0AFBA
  let whole_file_memory = new System.Memory<uint8>(font)
  let whole_file_checksum = calc_table_checksum whole_file_memory

  let head_record: TableRecord = Array.find (fun r -> r.tag = calc_tag "head") records
  r.BaseStream.Seek(int64 head_record.offset, System.IO.SeekOrigin.Begin)
  |> ignore
  // let head = Model.Header.read_head r
  let expected_checksum = 0xB1B0AFBAu
  if whole_file_checksum <> expected_checksum then
    failwithf "Checksum mismatch for the whole file: expected %X, got %X" expected_checksum whole_file_checksum
