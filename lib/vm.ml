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

let pop_stack vm = Stack.pop vm.stack

let push_stack vm value = Stack.push value vm.stack

let binary_op vm op =
  let b = pop_stack vm in
  let a = pop_stack vm in
  push_stack vm (op a b)

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
    let return_value = pop_stack vm in
    print_value return_value;
    print_newline ();
    Ok
  | Constant ->
    let constant = read_constant vm in
    push_stack vm constant;
    interpret_acc vm
  | LongConstant ->
    let constant = read_long_constant vm in
    push_stack vm constant;
    interpret_acc vm
  | Add -> binary_op vm (+.); interpret_acc vm
  | Subtract -> binary_op vm (-.); interpret_acc vm
  | Multiply -> binary_op vm Float.mul; interpret_acc vm
  | Divide -> binary_op vm (/.); interpret_acc vm
  | Negate ->
    pop_stack vm
    |> (~-.)
    |> (fun v -> Stack.push v vm.stack);
    interpret_acc vm

let interpret chunk =
  let state = { ip = 0; chunk; stack = Stack.create () } in
  interpret_acc state