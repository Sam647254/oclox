open OClox.Chunk
open OClox.Compiler
open OClox.Vm

let chunk =
  create_chunk
    [ Opcode Constant;
      Value 0;
      Opcode Constant;
      Value 1;
      Opcode Add;
      Opcode Constant;
      Value 2;
      Opcode Divide;
      Opcode Negate;
      Opcode Return ]
    (Float.Array.of_list [1.2; 3.4; 5.6]) 

let () =
  let _ =
    compile "print 1 + 2;"
    |> Result.map interpret in
  ()