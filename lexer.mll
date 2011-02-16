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
    [ ("test", (fun i -> TEST i))
    ; ("matches", (fun i -> MATCHES i))
    ]
}

let whitespace = [' ' '\t']+
let newline = "\n"
let uidchar = ['A'-'Z']
let idfirstchar = ['a'-'z' 'A'-'Z' '\'' '_' '-' '@']
let idchar = ['a'-'z' 'A'-'Z' '0'-'9' '\'' '_' '-' '@']
let intchar = ['0' - '9']
let hexchar = ['0' - '9' 'A' - 'E' 'a' - 'e']
let string = '"' [^'"']* '"'

rule top = parse
| whitespace         { top lexbuf }
| "("                { LPAREN(info lexbuf) }
| ")"                { RPAREN(info lexbuf) }
| "."                { DOT(info lexbuf) }
| "="                { EQUALS(info lexbuf) }
| ","                { COMMA(info lexbuf) }
| "*"                { STAR(info lexbuf) }
| "-"                { MINUS(info lexbuf) }
| "+"                { PLUS(info lexbuf) }
| "|"                { BAR(info lexbuf) }
| "{"                { LBRACE(info lexbuf) }
| "}"                { RBRACE(info lexbuf) }
| "<"                { LANGLE(info lexbuf) }
| ">"                { RANGLE(info lexbuf) }
| "?"                { QMARK(info lexbuf) }
| "\""               { let i1 = info lexbuf in 
                       let i2,s = string "" lexbuf in 
                       let i = Info.imerge i1 i2 in 
                       STR(i,s) }
| idfirstchar idchar* as ident { 
      try let kw = Hashtbl.find keywords ident in
          kw (info lexbuf)
      with Not_found -> 
        error lexbuf "unknown token" }
| intchar+ as integ { INTEGER(info lexbuf, int_of_string integ) }
| newline            { newline lexbuf; top lexbuf }
| eof                { EOF(info lexbuf) } 
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

and escape el = parse
| "\\"          { "\\" }
| "b"           { "\008" }
| "n"           { "\010" }
| "r"           { "\013" }
| "t"           { "\009" }
| "0x" (hexchar as h1) (hexchar as h2)
                { let int_of_hex = function
                    | '0' -> 0 | '1' -> 1 | '2' -> 2 | '3' -> 3 | '4' -> 4
                    | '5' -> 5 | '6' -> 6 | '7' -> 7 | '8' -> 8 | '9' -> 9
                    | 'A' | 'a' -> 10 | 'B' | 'b' -> 11 | 'C' | 'c' -> 12 
                    | 'D' | 'd' -> 13 | 'E' | 'e' -> 14 | 'F' | 'f' -> 15 
                    | _ -> error lexbuf "in escape sequence" in 
                  String.make 1 (Char.chr (16 * int_of_hex h1 + int_of_hex h2))
                }
| intchar intchar intchar as c 
                { String.make 1 (Char.chr (int_of_string c)) }
| _             { try 
                    Data.List.assoc (L.lexeme lexbuf) el 
                  with Not_found -> 
                    error lexbuf "in escape sequence" }
