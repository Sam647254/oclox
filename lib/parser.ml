open Printf
open Lexer
open Chunk

type precedence =
  | Bottom
  | Assignment
  | Or
  | And
  | Equality
  | Comparison
  | Term
  | Factor
  | Unary
  | Call
  | Primary

let precedence_of pred =
  match pred with
  | Bottom -> 0
  | Assignment -> 1
  | Or -> 2
  | And -> 3
  | Equality -> 4
  | Comparison -> 5
  | Term -> 6
  | Factor -> 7
  | Unary -> 8
  | Call -> 9
  | Primary -> 10

let next_precedence pred =
  match pred with
  | Bottom -> Assignment
  | Assignment -> Or
  | Or -> And
  | And -> Equality
  | Equality -> Comparison
  | Comparison -> Term
  | Term -> Factor
  | Factor -> Unary
  | Unary -> Call
  | Call -> Primary
  | Primary -> failwith "Primary has no next precedence"

type parse_fn = token list -> (token list, string) result
type parse_rule = parse_fn option * parse_fn option * precedence

let parse tokens =
  let byte_list = ref [] in
  let lines = ref [] in
  let constants = ref [] in

  let rec emit_constant c line =
    byte_list :=
    (char_of_int (List.length !constants))
        :: (opcode_to_byte Constant)
        :: !byte_list;
    constants := c :: !constants;
    lines := line :: !lines
  
  and emit_byte opcode =
    byte_list := (opcode_to_byte opcode) :: !byte_list
  
  and consume token_type error_message tokens =
    match tokens with
    | token :: rest when token.token_type = token_type ->
      Ok rest
    | _ -> Error error_message

  and get_rule token: parse_rule =
    match token with
    | LeftParen -> Some parse_grouping, None, Bottom
    | Minus -> Some parse_unary, Some parse_binary, Term
    | Plus -> None, Some parse_binary, Term
    | Star | Slash -> None, Some parse_binary, Factor
    | Number _ -> Some parse_number, None, Bottom
    | _ -> None, None, Bottom
  
  and parse_infix_precedence precedence tokens =
    match tokens with
    | token :: _ ->
      let (_, infix_rule, token_precedence) = get_rule token.token_type in
      if precedence <= token_precedence then
        match infix_rule with
        | Some rule ->
            Result.bind
              (rule tokens)
              (parse_infix_precedence precedence)
        | None -> Error (sprintf "Unable to parse %s" (token_type_to_string token.token_type))
      else
        Ok tokens
    | [] -> Ok []
  
  and parse_precedence precedence tokens: (token list, string) result =
    match tokens with
    | token :: _ ->
      (let (prefix_rule, _, _)= get_rule token.token_type in
      match prefix_rule with
      | Some rule ->
          Result.bind
            (rule tokens)
            (parse_infix_precedence precedence)
      | None -> Error (sprintf "Unexpected token: %s" (token_type_to_string token.token_type)))
    | [] -> Error "Unexpected end of input while parsing"
  
  and parse_expression tokens =
    parse_precedence Assignment tokens
  
  and parse_unary tokens =
    match tokens with
    | op_token :: tokens when op_token.token_type = Minus ->
      parse_precedence Unary tokens
      |> Result.map (fun remaining -> emit_byte Negate; remaining)
    | _ -> failwith "Unreachable"
  
  and parse_binary tokens =
    match tokens with
    | op_token :: tokens ->
      let (_, _, precedence) = get_rule op_token.token_type in
      parse_precedence (next_precedence precedence) tokens
      |> Result.map (fun remaining ->
        (match op_token.token_type with
        | Plus -> emit_byte Add
        | Minus -> emit_byte Subtract
        | Star -> emit_byte Multiply
        | Slash -> emit_byte Divide
        | _ -> failwith "Unreachable");
        remaining)
    | [] -> Error "Unexpected end of input while parsing binary expression"
  
  and parse_grouping tokens =
    Result.bind
      (parse_expression tokens)
      (consume RightParen "Expected ')' after expression.")

  and parse_number tokens =
    match tokens with
    | token :: tokens ->
      (match token.token_type with
      | Number n -> emit_constant n token.line; Ok tokens
      | _ -> Error (sprintf "Line %d: Expected number; got %s"
                      token.line (token_type_to_string token.token_type)))
    | [] -> Error "Unexpected end of input while parsing number"

  in
  Result.bind
    (parse_expression tokens)
    (fun remaining_tokens ->
      if remaining_tokens <> [] then
        Error "Leftover tokens"
      else
        (emit_byte Return;
        Ok ({
          chunk_bytes = !byte_list |> List.rev |> List.to_seq |> Bytes.of_seq;
          lines = !lines |> List.rev |> Array.of_list;
          constants = !constants |> List.rev |> Float.Array.of_list
        })))