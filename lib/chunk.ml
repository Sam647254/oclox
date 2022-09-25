open Printf

exception Unknown_bytecode of char

type opcode =
  | Return
  | Constant
  | LongConstant
  | Negate
  | Add
  | Subtract
  | Multiply
  | Divide

type vm_byte =
  | Opcode of opcode
  | Value of int

type chunk =
  { chunk_bytes: bytes;
    lines: int array;
    constants: floatarray }

let opcode_byte_to_string (op: char) =
  match op with
  | '\x00' -> "Return"
  | '\x01' -> "Constant"
  | '\x02' -> "Long constant"
  | '\x03' -> "Negate"
  | '\x04' -> "Add"
  | _ -> sprintf "Unknown opcode %d" (Char.code op)

let opcode_byte_to_opcode (op: char) =
  match op with
  | '\x00' -> Return
  | '\x01' -> Constant
  | '\x02' -> LongConstant
  | '\x03' -> Negate
  | '\x04' -> Add
  | '\x05' -> Subtract
  | '\x06' -> Multiply
  | '\x07' -> Divide
  | _ -> raise (Unknown_bytecode op)

let opcode_to_byte (op: opcode) =
  match op with
  | Return -> '\x00'
  | Constant -> '\x01'
  | LongConstant -> '\x02'
  | Negate -> '\x03'
  | Add -> '\x04'
  | Subtract -> '\x05'
  | Multiply -> '\x06'
  | Divide -> '\x07'

let print_value constant =
  printf "%g" constant

let rec disassemble chunk byte_index single =
  if byte_index >= Bytes.length chunk.chunk_bytes then () else
  let () = printf "%04d " byte_index in
  let next_byte = Bytes.get chunk.chunk_bytes byte_index in
  let next_opcode = opcode_byte_to_opcode next_byte in
  match next_opcode with
  | Constant ->
    let constant_index = Bytes.get chunk.chunk_bytes (byte_index + 1) in
    let constant =
      Float.Array.get
        chunk.constants
        (Char.code constant_index) in
    printf "%-16s %4d '" (opcode_byte_to_string next_byte) (Char.code constant_index);
    print_value constant;
    printf "'\n";
    if single then () else disassemble chunk (byte_index + 2) single
  | LongConstant ->
    let constant_index =
      let byte1 = Bytes.get chunk.chunk_bytes (byte_index + 1) in
      let byte2 = Bytes.get chunk.chunk_bytes (byte_index + 2) in
      let byte3 = Bytes.get chunk.chunk_bytes (byte_index + 3) in
      ((Char.code byte1) lsl 16) lor ((Char.code byte2) lsl 8) lor (Char.code byte3) in
    let constant = Float.Array.get chunk.constants constant_index in
    printf "%-16s %4d '" (opcode_byte_to_string next_byte) constant_index;
    printf "%g'\n" constant;
    if single then () else disassemble chunk (byte_index + 4) single
  | _ ->
    printf "%s\n" (opcode_byte_to_string next_byte);
    if single then () else disassemble chunk (byte_index + 1) single


let print_disassemble (name: string) (chunk: chunk) =
  printf "== %s ==\n" name;
  disassemble chunk 0 false;
  print_endline "== end =="

let vm_byte_to_byte vm_byte =
  match vm_byte with
  | Opcode op -> opcode_to_byte op
  | Value value -> Char.chr value

let create_chunk vm_bytes constants =
  let vm_chars = List.map vm_byte_to_byte vm_bytes in
  { chunk_bytes = Bytes.of_seq (List.to_seq vm_chars);
    lines = [|0; List.length vm_bytes|];
    constants }