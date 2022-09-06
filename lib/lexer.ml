open Printf

type token_type =
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | DoublePlus
  | Semicolon
  | Slash
  | Star
  | Bang
  | BangEqual
  | Equal
  | DoubleEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | Identifier of string
  | String of string
  | Number of float
  | And
  | Struct
  | Else
  | False
  | For
  | Fun
  | If
  | Nil
  | Or
  | Print
  | Return
  | True
  | Var
  | While
  | EOF

type token = {
  token_type: token_type;
  lexeme: string;
  line: int
}

type lexer = {
  stream: char Stream.t;
  mutable line: int;
}

let token_type_to_string token_type =
  match token_type with
  | LeftParen -> "LeftParen"
  | RightParen -> "RightParen"
  | LeftBrace -> "LeftBrace"
  | RightBrace -> "RightBrace"
  | Comma -> "Comma"
  | Dot -> "Dot"
  | Minus -> "Minus"
  | Plus -> "Plus"
  | DoublePlus -> "DoublePlus"
  | Semicolon -> "Semicolon"
  | Slash -> "Slash"
  | Star -> "Star"
  | Bang -> "Bang"
  | BangEqual -> "BangEqual"
  | Equal -> "Equal"
  | DoubleEqual -> "DoubleEqual"
  | Greater -> "Greater"
  | GreaterEqual -> "GreaterEqual"
  | Less -> "Less"
  | LessEqual -> "LessEqual"
  | Identifier _ -> "Identifier"
  | String _ -> "String"
  | Number _ -> "Number"
  | And -> "And"
  | Struct -> "Struct"
  | Else -> "Else"
  | False -> "False"
  | For -> "For"
  | Fun -> "Fun"
  | If -> "If"
  | Nil -> "Nil"
  | Or -> "Or"
  | Print -> "Print"
  | Return -> "Return"
  | True -> "True"
  | Var -> "Val"
  | While -> "While"
  | EOF -> "EOF"

let make_token lexer lexeme token_type =
  { token_type;
    lexeme;
    line = lexer.line; }

let expect_equal lexer c no_equal equal =
  match Stream.peek lexer.stream with
  | Some '=' ->
    Stream.junk lexer.stream;
    Ok (make_token lexer (String.of_seq (List.to_seq [c; '='])) equal)
  | _ -> Ok (make_token lexer (String.make 1 c) no_equal)

let rec skip_whitespace lexer =
  match Stream.peek lexer.stream with
  | Some c ->
    (match c with
    | ' ' | '\r' | '\t' ->
      Stream.junk lexer.stream;
      skip_whitespace lexer
    | _ -> ())
  | _ -> ()

let rec skip_comment lexer =
  match Stream.peek lexer.stream with
  | None -> ()
  | Some '\n' -> ()
  | Some _ -> skip_comment lexer

let read_string lexer =
  let rec read_string_acc buffer =
    match Stream.peek lexer.stream with
    | None 
    | Some '\n' -> Error (sprintf "Unterminated string on line %d" lexer.line)
    | Some '"' ->
      Stream.junk lexer.stream;
      Ok (Buffer.contents buffer)
    | Some c ->
      Buffer.add_char buffer c;
      read_string_acc buffer
  in
  read_string_acc (Buffer.create 80)

let read_number first lexer =
  let rec read_number_acc buffer whole =
    match Stream.peek lexer.stream with
    | Some c ->
      (match c with
      | '0'..'9' ->
        Buffer.add_char buffer (Stream.next lexer.stream);
        read_number_acc buffer whole
      | '.' ->
        if whole then
          (Buffer.add_char buffer (Stream.next lexer.stream);
          read_number_acc buffer false)
        else
          Ok (Buffer.contents buffer)
      | _ -> Ok (Buffer.contents buffer))
    | None -> Ok (Buffer.contents buffer)
  in
  let buffer = Buffer.create 20 in
  Buffer.add_char buffer first;
  read_number_acc buffer true

let read_identifier first lexer =
  let rec read_identifier_acc buffer whole =
    match Stream.peek lexer.stream with
    | Some c ->
      (match c with
      | 'a'..'z' | 'A'..'Z' | '.' ->
        Buffer.add_char buffer (Stream.next lexer.stream);
        read_identifier_acc buffer whole
      | _ -> Ok (Buffer.contents buffer))
    | None -> Ok (Buffer.contents buffer)
  in
  let buffer = Buffer.create 20 in
  Buffer.add_char buffer first;
  read_identifier_acc buffer true

let to_keyword lexer id =
  let token_type =
    match id with
    | "and" -> And
    | "struct" -> Struct
    | "else" -> Else
    | "if" -> If
    | "nil" -> Nil
    | "or" -> Or
    | "print" -> Print
    | "return" -> Return
    | "var" -> Var
    | "while" -> While
    | "false" -> False
    | "for" -> For
    | "fun" -> Fun
    | _ -> Identifier id
  in
  { token_type;
    line = lexer.line;
    lexeme = id }

let rec scan_token lexer =
  skip_whitespace lexer;
  match Stream.peek lexer.stream with
  | None -> Ok { token_type = EOF; lexeme = ""; line = lexer.line }
  | Some _ ->
    let c = Stream.next lexer.stream in
    match c with
      | '(' -> Ok (make_token lexer "(" LeftParen)
      | ')' -> Ok (make_token lexer ")" RightParen)
      | '{' -> Ok (make_token lexer "{" LeftBrace)
      | '}' -> Ok (make_token lexer "}" RightBrace)
      | ';' -> Ok (make_token lexer ";" Semicolon)
      | ',' -> Ok (make_token lexer "," Comma)
      | '.' -> Ok (make_token lexer "." Dot)
      | '-' -> Ok (make_token lexer "-" Minus)
      | '+' ->
        (match Stream.peek lexer.stream with
        | Some '+' ->
          Stream.junk lexer.stream;
          Ok (make_token lexer "++" RightParen)
        | _ -> Ok (make_token lexer "+" Plus))
      | '/' ->
        (match Stream.peek lexer.stream with
        | Some '/' ->
          skip_comment lexer;
          scan_token lexer
        | _ -> Ok (make_token lexer (String.make 1 c) RightParen))
      | '*' -> Ok (make_token lexer (String.make 1 c) RightParen)
      | '!' -> expect_equal lexer c Bang BangEqual
      | '=' -> expect_equal lexer c Equal DoubleEqual
      | '<' -> expect_equal lexer c Less LessEqual
      | '>' -> expect_equal lexer c Greater GreaterEqual
      | '"' ->
        read_string lexer
        |> Result.map (fun str -> make_token lexer str (String str))
      | '0'..'9' ->
        read_number c lexer
        |> Result.map (fun str ->
            let number = float_of_string str in
            make_token lexer str (Number number))
      | 'a'..'z' | 'A'..'Z' | '_' ->
        read_identifier c lexer
        |> Result.map (to_keyword lexer)
      | '\n' ->
        lexer.line <- lexer.line + 1;
        scan_token lexer
      | c -> Error (sprintf "Unexpected character %c on line %d" c lexer.line)

let rec scan_acc lexer tokens =
  match scan_token lexer with
  | Ok token ->
    if token.token_type = EOF then
      Ok (List.rev tokens)
    else
      scan_acc lexer (token :: tokens)
  | Error _ as e -> e

let scan_source source =
  let lexer =
    { stream = Stream.of_string source;
      line = 1 } in
  scan_acc lexer []

let scan input_file =
  let lexer =
    { stream = Stream.of_channel input_file;
      line = 1 } in
  scan_acc lexer []

let print_tokens source =
  let lexer =
    { stream = Stream.of_string source;
      line = 1 } in
  let rec print_tokens_acc lexer =
    let line = ref ~-1 in
    match scan_token lexer with
    | Ok token ->
      (if token.line <> !line then
        (printf "%4d " token.line;
        line := token.line)
      else
        printf "   | ");
      printf "%16s '%s'\n" (token_type_to_string token.token_type) token.lexeme;
      if token.token_type == EOF then () else
      print_tokens_acc lexer
    | Error message -> print_endline message
  in
  print_tokens_acc lexer