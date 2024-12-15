/// Instructions for the TrueType bytecode interpreter.
module Rynco.KuiperSans.Model.Inst

open System

/// Count of push/popped (in slots) or data after the instruction (in bytes).
type DataCount =
  | Fixed of int
  /// Variable number calculated from instruction byte and the follwing data in
  /// the instruction stream.
  | Variable of (int -> ReadOnlyMemory<uint8> -> int)

type InstructionDescriptor = {
  opcode: uint8
  range_lower_bits: int
  name: string
  push_count: DataCount
  pop_count: DataCount
  data: DataCount
}

let inst opcode name push pop data = {
  opcode = uint8 opcode
  range_lower_bits = 0
  name = name
  push_count = Fixed push
  pop_count = Fixed pop
  data = Fixed data
}

let inst' opcode range_lower_bits name push pop data = {
  opcode = uint8 opcode
  range_lower_bits = range_lower_bits
  name = name
  push_count = push
  pop_count = pop
  data = data
}

let npush_u8_push_count _ (data: uint8 ReadOnlyMemory) = int data.Span[0]
let npush_u8_data_bytes _ (data: uint8 ReadOnlyMemory) = int (data.Span[0]) + 1
let npush_u16_push_count _ (data: uint8 ReadOnlyMemory) = int data.Span[0]
let npush_u16_data_bytes _ (data: uint8 ReadOnlyMemory) = int (data.Span[0]) * 2 + 1

let push_u8_push_count opcode _ = int (opcode - 0xb0) + 1
let push_u8_data_bytes opcode _ = int (opcode - 0xb0) + 1
let push_u16_push_count opcode _ = int (opcode - 0xb8) + 1
let push_u16_data_bytes opcode _ = int (opcode - 0xb8) * 2 + 1

let raw_instructions = [|
  // Pushing data onto the interpreter stack
  // Push n unsigned bytes.
  inst' 0x40 0 "NPushU8" (Variable npush_u8_push_count) (Fixed 0) (Variable npush_u8_data_bytes)
  // Push n unsigned 16-bit integers encoded in big-endian.
  inst' 0x41 0 "NPushU16" (Variable npush_u16_push_count) (Fixed 0) (Variable npush_u16_data_bytes)
  // Push `n` unsigned bytes, where `n-1` is the lowest 3 bits of the opcode.
  inst' 0xb0 3 "PushU8" (Variable push_u8_push_count) (Fixed 0) (Variable push_u8_data_bytes)
  // Push `n` unsigned 16-bit integers encoded in big-endian, where `n-1` is the
  // lowest 3 bits of the opcode.
  inst' 0xb8 3 "PushU16" (Variable push_u16_push_count) (Fixed 0) (Variable push_u16_data_bytes)

  // Managing the store area
  // Read a 32-bit value from the store area.
  // -> location: u32
  // <- Store[location]: u32
  inst 0x43 "ReadStore" 0 1 0
  // Write a 32-bit value to the store area.
  // -> value: u32
  // -> location: u32
  //    Store[location] = value
  inst 0x42 "WriteStore" 2 0 0

  // Manage the Control Value Table
  // Writes a value into the control value table, in pixel units.
  // -> value: f26dot6
  // -> location: u32
  //    CVT[location] = value
  inst 0x44 "WriteCVTPixel" 2 0 0
  // Writes a value into the control value table, in font design unit.
  // The value is scaled before being written.
  // -> value: u32
  // -> location: u32
  //    CVT[location] = scaled(value)
  inst 0x70 "WriteCVTDesign" 2 0 0
  // Read a value from the control value table, in pixel units.
  // -> location: u32
  // <- CVT[location]: f26dot6
  inst 0x45 "ReadCVTPixel" 1 1 0

  // Managing the graphics state
  // Projection vector: in which direction do we measure distances?
  // Freedom vector: in which direction do we move things?
  // Moves along the freedom vector is measured in the projection vector.
  // Zones:
  // Zone 1 is the glyph outline
  // Zone 0 (twilight zone) is a scratch space for calculations

  // Set both freedom and projection vectors to axes
  // projection_vector = (0, 1)
  // freedom_vector = (1, 0)
  inst 0x00 "SetVectorToCoordAxisY" 0 0 0
  // projection_vector = (1, 0)
  // freedom_vector = (0, 1)
  inst 0x01 "SetVectorToCoordAxisX" 0 0 0
  // Set projection vector to axes
  // projection_vector = (0, 1)
  inst 0x02 "SetProjectionVectorToCoordAxisY" 0 0 0
  // projection_vector = (1, 0)
  inst 0x03 "SetProjectionVectorToCoordAxisX" 0 0 0
  // Set freedom vector to axes
  // freedom_vector = (0, 1)
  inst 0x04 "SetFreedomVectorToCoordAxisY" 0 0 0
  // freedom_vector = (1, 0)
  inst 0x05 "SetFreedomVectorToCoordAxisX" 0 0 0
  // Set the projection vector to be along a specific line
  // Parallel to a specific line
  // -> idx1: u32
  // -> idx2: u32
  //    projection_vector = zp2[idx2] - zp1[idx1]
  inst 0x06 "SetProjectionVectorParallelTo" 0 2 0
  // Set the projection vector to be perpendicular to a specific line
  // Perpendicular to a specific line
  // -> idx1: u32
  // -> idx2: u32
  //    projection_vector = rot(-90, zp2[idx2] - zp1[idx1])
  inst 0x07 "SetProjectionVectorPerpendicularTo" 0 2 0
  // Set the freedom vector to be along a specific line
  // Parallel to a specific line
  // -> idx1: u32
  // -> idx2: u32
  //    freedom_vector = zp2[idx2] - zp1[idx1]
  inst 0x08 "SetFreedomVectorParallelTo" 0 2 0
  // Set the freedom vector to be perpendicular to a specific line
  // Perpendicular to a specific line
  // -> idx1: u32
  // -> idx2: u32
  //    freedom_vector = rot(-90, zp2[idx2] - zp1[idx1])
  inst 0x09 "SetFreedomVectorPerpendicularTo" 0 2 0

// TODO
|]
