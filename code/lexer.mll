{
(******************************************************************************)
(* The Frenetic Project                                                       *)
(* frenetic@frenetic-lang.org                                                 *)
(******************************************************************************)
(* Licensed to the Frenetic Project by one or more contributors. See the      *)
(* NOTICE file distributed with this work for additional information          *)
(* regarding copyright and ownership. The Frenetic Project licenses this      *)
(* file to you under the following license.                                   *)
(*                                                                            *)
(* Redistribution and use in source and binary forms, with or without         *)
(* modification, are permitted provided the following conditions are met:     *)
(* - Redistributions of source code must retain the above copyright           *)
(*   notice, this list of conditions and the following disclaimer.            *)
(* - Redistributions in binary form must reproduce the above copyright        *)
(*   notice, this list of conditions and the following disclaimer in          *)
(*   the documentation or other materials provided with the distribution.     *)
(* - The names of the copyright holds and contributors may not be used to     *)
(*   endorse or promote products derived from this work without specific      *)
(*   prior written permission.                                                *)
(*                                                                            *)
(* Unless required by applicable law or agreed to in writing, software        *)
(* distributed under the License is distributed on an "AS IS" BASIS, WITHOUT  *)
(* WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the   *)
(* LICENSE file distributed with this work for specific language governing    *)
(* permissions and limitations under the License.                             *)
(******************************************************************************)
(* /src/compiler/lexer.mll                                                    *)
(* Frenetic Lexer                                                             *)
(* $Id$ *)
(******************************************************************************)

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

let keywords = Hashtbl.create 4
let _ = 
  Data.List.iter (fun (kw,tok) -> Hashtbl.add keywords kw tok)
    [ ("and", (fun i -> AND i))
    ; ("or", (fun i -> OR i))
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
| "true"             { TRUE(info lexbuf) }
| "false"            { FALSE(info lexbuf) }
| "~"                { TILDE(info lexbuf) }
| "-"                { MINUS(info lexbuf) }
| "and"              { AND(info lexbuf) }
| "or"               { OR(info lexbuf) }
| ":="               { GETS(info lexbuf) }
| "*"                { STAR(info lexbuf) }
| ";"                { SEQ(info lexbuf) }
| "||"               { PAR(info lexbuf) }
| "skip"             { SKIP(info lexbuf) }
| "[]"               { THICKBAR(info lexbuf) }
| "|"                { THINBAR(info lexbuf) }
| "]"                { RBRACK(info lexbuf) }
| "["                { LBRACK(info lexbuf) }
| "->"               { ARROW(info lexbuf) }
| "!"                { BANG(info lexbuf) }
| "?"                { QMARK(info lexbuf) }
| "."                { DOT(info lexbuf) }
| "`a"               { CHANACK(info lexbuf) }
| "`t"               { CHANTRUE(info lexbuf) }
| "`f"               { CHANFALSE(info lexbuf) }
| id_char_first id_char_rest* as ident { 
  try let kw = Hashtbl.find keywords ident in
      kw (info lexbuf)
  with Not_found -> 
    if Char.uppercase ident.[0] = ident.[0] then 
      UIDENT (info lexbuf, ident)
    else 
      LIDENT (info lexbuf, ident) 
}
| newline            { newline lexbuf; main lexbuf }
| eof                { EOF(info lexbuf) } 
| "(*"               { comment lexbuf; main lexbuf }
| _                  { error lexbuf "unknown token" }

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
