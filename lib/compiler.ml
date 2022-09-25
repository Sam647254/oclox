open Printf
open Lexer
open Parser
open Chunk

let compile source: (chunk, string) result =
  Result.bind
    (scan_source source
    |> Result.map_error (sprintf "Error while scanning: %s"))
    parse