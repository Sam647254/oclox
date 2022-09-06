open Printf
open Lexer
open Chunk

let compile source: (chunk, string) result =
  scan_source source
  |> Result.map_error (sprintf "Error while scanning: %s")
  |> Result.map (failwith "TODO")