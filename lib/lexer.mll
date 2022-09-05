{
  type token =
  | Number of float
  | String of string
  | Identifier of string
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
}

rule read_token = parse
| [' ' '\t']+ { read_token lexbuf }
| ['0'-'9']+('.'['0'-'9']+)? as number { Number (float_of_string number) }
| '"'([^'\n']*)'"' as str { String str }