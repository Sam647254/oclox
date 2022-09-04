open Printf
open Chunk

type vm = {
  chunk: chunk;
  mutable ip: int;
  stack: float Stack.t
}

exception Unimplemented_bytecode of char

type interpret_result =
  | Ok
  | CompileError
  | RuntimeError

let print_stack vm =
  if Stack.is_empty vm.stack then
    printf "Empty stack\n"
  else
    printf "--- Stack ---\n";
    Stack.iter (printf "[ %g ]\n") vm.stack

let get_next_byte vm =
  let byte = Bytes.get vm.chunk.chunk_bytes vm.ip in
  vm.ip <- vm.ip + 1;
  byte

let read_constant vm =
  let index = get_next_byte vm in
  Float.Array.get vm.chunk.constants (Char.code index)

let read_long_constant vm =
  let index =
    let byte1 = get_next_byte vm in
    let byte2 = get_next_byte vm in
    let byte3 = get_next_byte vm in
    ((Char.code byte1) lsl 16) lor ((Char.code byte2) lsl 8) lor (Char.code byte3) in
  Float.Array.get vm.chunk.constants index

let rec interpret_acc vm =
  let () = print_stack vm in
  let () = disassemble vm.chunk vm.ip true in
  let instruction = get_next_byte vm in
  match opcode_byte_to_opcode instruction with
  | Return ->
    let return_value = Stack.pop vm.stack in
    print_value return_value;
    print_newline ();
    Ok
  | Constant ->
    let constant = read_constant vm in
    Stack.push constant vm.stack;
    interpret_acc vm
  | LongConstant ->
    let constant = read_long_constant vm in
    Stack.push constant vm.stack;
    interpret_acc vm
  | Negate ->
    Stack.pop vm.stack
    |> (~-.)
    |> (fun v -> Stack.push v vm.stack);
    interpret_acc vm

let interpret chunk =
  let state = { ip = 0; chunk; stack = Stack.create () } in
  interpret_acc state