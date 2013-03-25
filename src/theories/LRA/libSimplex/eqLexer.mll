{
  open EqParser

  exception LexError of string
}

let digit = ['0'-'9']
let id    = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']*
let space = [' ' '\t' '\r' '\n']

rule lexer = parse
  | digit+ as inum { INTEGER (Big_int.big_int_of_string inum) }
  | id+    as id   { ID id }

  | '+' { PLUS  }
  | '-' { MINUS }
  | '/' { DIV   }

  | "<=" { LE }
  | ">=" { GE }
  | "<"  { LT }
  | ">"  { GT }
(*  | "="  { EQ }*)

  | space+ { lexer lexbuf }

  | _ as c { raise (LexError (Printf.sprintf "invalid character: %c" c)) }
  | eof    { EOL }
