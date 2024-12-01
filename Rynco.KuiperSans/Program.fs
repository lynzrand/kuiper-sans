module Rynco.KuiperSans.Program

type table_dir = {
  version: uint32
  n_tables: uint16
  search_range: uint16
  entry_selector: uint16
  range_shift: uint16
}

type table_record = {
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
    sum <- sum + u32_tbl.[i]

  sum
