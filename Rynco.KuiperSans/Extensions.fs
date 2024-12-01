module Rynco.KuiperSans.Extensions

type BinaryWriter = System.IO.BinaryWriter
type BinPrim = System.Buffers.Binary.BinaryPrimitives

let is_le = System.BitConverter.IsLittleEndian

let to_be_16 (x: uint16) =
  if is_le then BinPrim.ReverseEndianness x else x
let to_be_32 (x: uint32) =
  if is_le then BinPrim.ReverseEndianness x else x
let to_be_64 (x: uint64) =
  if is_le then BinPrim.ReverseEndianness x else x

let to_be_i16 (x: int16) = to_be_16 (uint16 x)
let to_be_i32 (x: int32) = to_be_32 (uint32 x)
let to_be_i64 (x: int64) = to_be_64 (uint64 x)

let write_u8_be (writer: BinaryWriter) (value: byte) = writer.Write(value)
let write_u16_be (writer: BinaryWriter) (value: uint16) = writer.Write(to_be_16 value)
let write_u32_be (writer: BinaryWriter) (value: uint32) = writer.Write(to_be_32 value)
let write_u64_be (writer: BinaryWriter) (value: uint64) = writer.Write(to_be_64 value)
let write_i8_be (writer: BinaryWriter) (value: sbyte) = writer.Write(byte value)
let write_i16_be (writer: BinaryWriter) (value: int16) = writer.Write(to_be_i16 value)
let write_i32_be (writer: BinaryWriter) (value: int32) = writer.Write(to_be_i32 value)
let write_i64_be (writer: BinaryWriter) (value: int64) = writer.Write(to_be_i64 value)
