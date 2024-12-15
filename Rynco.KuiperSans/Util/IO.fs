module Rynco.KuiperSans.Util.IO

open Operators

type BinaryWriter = System.IO.BinaryWriter
type BinPrim = System.Buffers.Binary.BinaryPrimitives

let is_le = System.BitConverter.IsLittleEndian

let to_be_16 (x: uint16) =
  if is_le then BinPrim.ReverseEndianness x else x
let to_be_32 (x: uint32) =
  if is_le then BinPrim.ReverseEndianness x else x
let to_be_64 (x: uint64) =
  if is_le then BinPrim.ReverseEndianness x else x

let to_be_i16 (x: int16) = to_be_16 (uint16 x) |> int16
let to_be_i32 (x: int32) = to_be_32 (uint32 x) |> int32
let to_be_i64 (x: int64) = to_be_64 (uint64 x) |> int64

let write_u8_be (w: BinaryWriter) (value: byte) = w.Write(value)
let write_u16_be (w: BinaryWriter) (value: uint16) = w.Write(to_be_16 value)
let write_u32_be (w: BinaryWriter) (value: uint32) = w.Write(to_be_32 value)
let write_u64_be (w: BinaryWriter) (value: uint64) = w.Write(to_be_64 value)
let write_i8_be (w: BinaryWriter) (value: sbyte) = w.Write(byte value)
let write_i16_be (w: BinaryWriter) (value: int16) = w.Write(to_be_i16 value)
let write_i32_be (w: BinaryWriter) (value: int32) = w.Write(to_be_i32 value)
let write_i64_be (w: BinaryWriter) (value: int64) = w.Write(to_be_i64 value)
let write_fword (w: BinaryWriter) (value: Math.fword) = write_i16_be w value
let write_ufword (w: BinaryWriter) (value: Math.ufword) = write_u16_be w value

let read_u8_be (r: System.IO.BinaryReader) = r.ReadByte()
let read_u16_be (r: System.IO.BinaryReader) = to_be_16 (r.ReadUInt16())
let read_u32_be (r: System.IO.BinaryReader) = to_be_32 (r.ReadUInt32())
let read_u64_be (r: System.IO.BinaryReader) = to_be_64 (r.ReadUInt64())
let read_i8_be (r: System.IO.BinaryReader) = r.ReadSByte()
let read_i16_be (r: System.IO.BinaryReader) = to_be_i16 (r.ReadInt16())
let read_i32_be (r: System.IO.BinaryReader) = to_be_i32 (r.ReadInt32())
let read_i64_be (r: System.IO.BinaryReader) = to_be_i64 (r.ReadInt64())
let read_fword (r: System.IO.BinaryReader) = read_i16_be r
let read_ufword (r: System.IO.BinaryReader) = read_u16_be r


// String accumulator
//
// used to accumulate individual strings into a single buffer to be written
// after the actual table data

type StringAccumulator = {
  mem: System.IO.MemoryStream
  buf: System.IO.BinaryWriter
  encoding: System.Text.Encoding
  zero: byte array
}

let mk_string_accumulator encoding =
  let mem = new System.IO.MemoryStream()
  {
    mem = mem
    buf = new System.IO.BinaryWriter(mem)
    encoding = encoding
    zero = encoding.GetBytes("\0")
  }

/// Add a string to the accumulator. Returns the offset of the string in the
/// storage and the length of the string.
///
/// This function assumes that the accumulated string will not exceed the
/// maximum length of a int32. Should be pretty enough since the length in TTF
/// files are mostly limited to uint16.
let add_string (acc: StringAccumulator) (s: string) : int * int =
  let encoded_string = acc.encoding.GetBytes(s)
  let offset = int acc.buf.BaseStream.Position
  acc.buf.Write(encoded_string)

  // Just for informative purposes, we append a zero character to the end of the
  // string to make it easier to debug the font file.
  acc.buf.Write(acc.zero)

  offset, encoded_string.Length

let get_string_buf (acc: StringAccumulator) = acc.mem.GetBuffer()
