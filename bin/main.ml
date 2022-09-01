open OClox.Chunk

let () =
  let chunk =
    { chunk_bytes = Bytes.of_string "\x01\x00\x02\x00\x00\x01\x00";
      constants = Float.Array.of_list [1.2; 3.1];
      lines = [|123; 123; 123; 123; 123; 123; 123|] } in
  print_disassemble chunk "test chunk"