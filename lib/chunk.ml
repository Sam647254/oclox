open Printf

exception Unknown_bytecode of char

type opcode =
  | Return
  | Constant
  | LongConstant

type chunk =
  { chunk_bytes: bytes;
    lines: int array;
    constants: floatarray }

let opcode_byte_to_string (op: char) =
  match op with
  | '\x00' -> "Return"
  | '\x01' -> "Constant"
  | '\x02' -> "Long constant"
  | _ -> "Unknown opcode"

let opcode_byte_to_opcode (op: char) =
  match op with
  | '\x00' -> Return
  | '\x01' -> Constant
  | '\x02' -> LongConstant
  | _ -> raise (Unknown_bytecode op)

let opcode_to_byte (op: opcode) =
  match op with
  | Return -> '\x00'
  | Constant -> '\x01'
  | LongConstant -> '\x02'

let rec disassemble chunk byte_index =
  if byte_index >= Bytes.length chunk.chunk_bytes then () else
  let () = printf "%04d " byte_index in
  let next_byte = Bytes.get chunk.chunk_bytes byte_index in
  let next_line = Array.get chunk.lines byte_index in
  let () =
    if byte_index > 0 && next_line = Array.get chunk.lines (byte_index - 1) then
      printf "   | "
    else
      printf "%4d " next_line
    in
  let next_opcode = opcode_byte_to_opcode next_byte in
  match next_opcode with
  | Constant ->
    let constant_index = Bytes.get chunk.chunk_bytes (byte_index + 1) in
    let constant =
      Float.Array.get
        chunk.constants
        (Char.code constant_index) in
    let () = printf "%-16s %4d '" (opcode_byte_to_string next_byte) (Char.code constant_index) in
    let () = printf "%g'\n" constant in
    disassemble chunk (byte_index + 2)
  | LongConstant ->
    let constant_index =
      let byte1 = Bytes.get chunk.chunk_bytes (byte_index + 1) in
      let byte2 = Bytes.get chunk.chunk_bytes (byte_index + 2) in
      let byte3 = Bytes.get chunk.chunk_bytes (byte_index + 3) in
      ((Char.code byte1) lsl 16) lor ((Char.code byte2) lsl 8) lor (Char.code byte3) in
    let constant = Float.Array.get chunk.constants constant_index in
    let () = printf "%-16s %4d '" (opcode_byte_to_string next_byte) constant_index in
    let () = printf "%g'\n" constant in
    disassemble chunk (byte_index + 4)
  | _ ->
    let () = printf "%s\n" (opcode_byte_to_string next_byte) in
    disassemble chunk (byte_index + 1)


let print_disassemble (chunk: chunk) (name: string) =
  let () = printf "== %s ==\n" name in
  disassemble chunk 0