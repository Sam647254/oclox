open OClox.Chunk
open OClox.Vm

let () =
  let chunk =
    { chunk_bytes = Bytes.of_string "\x01\x00\x03\x00";
      constants = Float.Array.of_list [1.2];
      lines = [|123; 6|] } in
  let _ = interpret chunk in ()