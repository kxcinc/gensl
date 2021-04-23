type tmptok = [`Cat | `Dog | `Space | `SomeBool | `Eof]
[@@deriving sexp]

let rec lexer buf = match%sedlex buf with
  | "cat" -> `Cat
  | "dog" -> `Dog
  | ("true" | "false") -> `SomeBool
  | white_space -> lexer buf
  | eof -> `Eof
  | _ -> failwith "invalid tok"
