{
open Parser

module L = Lexing
let sprintf = Printf.sprintf

(* Stack of lexing information. *)
let info_stk = ref []

let peek_info_stk () = match !info_stk with 
  | h::t -> (h,t)
  | _ -> Error.simple_error "Lexer.filename : info stack is empty."

let filename () = 
  let (fn,_,_),_ = peek_info_stk () in 
  fn

let lineno () = 
  let (_,l,_),_ = peek_info_stk () in 
  l 

let linestart () = 
  let (_,_,c),_ = peek_info_stk () in 
  c

let set_filename fn = 
  let (_,l,c),t = peek_info_stk () in
  info_stk := (fn,l,c)::t

let set_lineno l = 
  let (fn,_,c),t = peek_info_stk () in
  info_stk := (fn,l,c)::t

let set_linestart c = 
  let (fn,l,_),t = peek_info_stk () in
  info_stk := (fn,l,c)::t

let setup fn = 
  info_stk := (fn,1,0)::!info_stk

let finish () = 
  let _,t = peek_info_stk () in 
  info_stk := t

let newline lexbuf : unit = 
  set_linestart (L.lexeme_start lexbuf);
  set_lineno (lineno () + 1)

let info lexbuf : Info.t = 
  let c1 = L.lexeme_start lexbuf in
  let c2 = L.lexeme_end lexbuf in
  let l = lineno () in
  let c = linestart () in
  Info.I (filename(), (l, c1 - c),(l, c2 - c))

let error lexbuf s =
  Error.simple_error 
    (sprintf "%s: lexing error %s at %s." 
       (Info.string_of_t (info lexbuf))
       s
       (L.lexeme lexbuf))

let keywords = Hashtbl.create 31
let _ = 
  Data.List.iter (fun (kw,tok) -> Hashtbl.add keywords kw tok)
    [ ("module", (fun i -> MODULE i))
    ; ("let", (fun i -> LET i)) 
    ; ("in", (fun i -> IN i))
    ; ("fun", (fun i -> FUN i))
    ; ("begin", (fun i -> BEGIN i))
    ; ("end", (fun i -> END i))
    ; ("and", (fun i -> AND i))
    ; ("test", (fun i -> TEST i))
    ; ("match", (fun i -> MATCH i))
    ; ("with", (fun i -> WITH i))
    ; ("error", (fun i -> ERROR i))
    ; ("char", (fun i -> CHAR i))
    ; ("string", (fun i -> STRING i))
    ; ("int", (fun i -> INT i))
    ; ("bool", (fun i -> BOOL i))
    ; ("unit", (fun i -> UNIT i))
    ; ("type", (fun i -> TYPE i))
    ; ("of", (fun i -> OF i))
    ; ("where", (fun i -> WHERE i))
    ; ("forall", (fun i -> FORALL i))
    ; ("lt", (fun i -> LT i))
    ; ("leq", (fun i -> LEQ i))
    ; ("gt", (fun i -> GT i))
    ; ("geq", (fun i -> GEQ i))
    ; ("true", (fun i -> BOOLEAN(i,true)))
    ; ("false", (fun i -> BOOLEAN(i,false)))
    ]
}

let whitespace = [' ' '\t']+
let newline = "\n"
let uid_char = ['A'-'Z']
let id_char_first = ['a'-'z' 'A'-'Z' '\'' '_' '-' '@']
let id_char_rest = ['a'-'z' 'A'-'Z' '0'-'9' '\'' '_' '-' '@']
let int_char = ['0' - '9']
let hex_char = ['0' - '9' 'A' - 'E' 'a' - 'e']
let string = '"' [^'"']* '"'

rule main = parse
| whitespace         { main lexbuf }
| "*)"               { error lexbuf "this is not the end of a comment" }
| "("                { LPAREN(info lexbuf) }
| ")"                { RPAREN(info lexbuf) }
| ";"                { SEMI(info lexbuf) }
| "."                { DOT(info lexbuf) }
| "&"                { AMPERSAND(info lexbuf) }
| "*"                { STAR(info lexbuf) }
| "-"                { MINUS(info lexbuf) }
| "_"                { UNDERLINE(info lexbuf) }
| "$"                { DOLLAR(info lexbuf) }
| "+"                { PLUS(info lexbuf) }
| "!"                { BANG(info lexbuf) }
| "->"               { ARROW(info lexbuf) }
| "=>"               { EQARROW(info lexbuf) }
| "<=>"              { DEQARROW(info lexbuf) }
| "<->"              { DARROW(info lexbuf) }
| "|"                { BAR(info lexbuf) }
| "="                { EQUAL(info lexbuf) }
| "{"                { LBRACE(info lexbuf) }
| "}"                { RBRACE(info lexbuf) }
| "#"                { HASH(info lexbuf) }
| "]"                { RBRACK(info lexbuf) }
| "["                { LBRACK(info lexbuf) }
| "<"                { LANGLE(info lexbuf) }
| ">"                { RANGLE(info lexbuf) }
| ","                { COMMA(info lexbuf) }
| ":"                { COLON(info lexbuf) }
| "^"                { HAT(info lexbuf) }
| "~"                { TILDE(info lexbuf) }
| "\\"               { BACKSLASH(info lexbuf) }
| "?"                { QMARK(info lexbuf) }
| "\""               { let i1 = info lexbuf in 
                       let i2,s = string "" lexbuf in 
                       let i = Info.imerge i1 i2 in 
                       STR(i,s) }

| "'" ([^'\''] as c) "'" { 
    CHARACTER(info lexbuf,c) 
}

| "'\\" { 
  let c = escape [("'","'")] lexbuf in 
  character c lexbuf 
}

| '\'' (id_char_first id_char_rest* as ident) { 
  TYVARIDENT(info lexbuf, ident)
}
| id_char_first id_char_rest* as ident { 
  try let kw = Hashtbl.find keywords ident in
      kw (info lexbuf)
  with Not_found -> 
    if Char.uppercase ident.[0] = ident.[0] then 
      UIDENT (info lexbuf, ident)
    else 
      LIDENT (info lexbuf, ident) 
}
| (uid_char id_char_rest* ".")+ id_char_rest+ as qident {
  QUALIDENT(info lexbuf,qident)
}
| int_char+ as integ { 
  INTEGER(info lexbuf, int_of_string integ) 
}
| (int_char* "." int_char+) as flot {
  FLOAT(info lexbuf, float_of_string flot) 
} 
| newline            { newline lexbuf; main lexbuf }
| eof                { EOF(info lexbuf) } 
| "(*"               { comment lexbuf; main lexbuf }
| _                  { error lexbuf "unknown token" }

and string acc = parse
| "\\"          { let s = escape [("\"","\"");("'","'")] lexbuf in 
                  string (acc ^ s) lexbuf }
| "\""          { (info lexbuf,acc) }
| newline ([' ' '\t']* "|")? 
                { newline lexbuf; 
                  string (acc ^ "\n") lexbuf}
| eof           { error lexbuf "unmatched '\"'"}
| _             { string (acc ^ L.lexeme lexbuf) lexbuf }

and character acc = parse 
  | "'"         { if String.length acc <> 1 then error lexbuf "unmatched '''"
                  else CHARACTER(info lexbuf,acc.[0]) }

  | _           { error lexbuf "unmatched '''" }

and escape el = parse
| "\\"          { "\\" }
| "b"           { "\008" }
| "n"           { "\010" }
| "r"           { "\013" }
| "t"           { "\009" }
| "0x" (hex_char as h1) (hex_char as h2)
                { let int_of_hex = function
                    | '0' -> 0 | '1' -> 1 | '2' -> 2 | '3' -> 3 | '4' -> 4
                    | '5' -> 5 | '6' -> 6 | '7' -> 7 | '8' -> 8 | '9' -> 9
                    | 'A' | 'a' -> 10 | 'B' | 'b' -> 11 | 'C' | 'c' -> 12 
                    | 'D' | 'd' -> 13 | 'E' | 'e' -> 14 | 'F' | 'f' -> 15 
                    | _ -> error lexbuf "in escape sequence" in 
                  String.make 1 (Char.chr (16 * int_of_hex h1 + int_of_hex h2))
                }

| int_char int_char int_char as c 
                { String.make 1 (Char.chr (int_of_string c)) }
| _             { try Data.List.assoc (L.lexeme lexbuf) el 
                  with Not_found -> 
                    error lexbuf "in escape sequence" }

and comment = parse
| "(*"             { comment lexbuf; comment lexbuf }
| "*)"             { () }
| newline          { newline lexbuf; comment lexbuf }
| eof              { error lexbuf "unmatched '(*'" }
| _                { comment lexbuf }
