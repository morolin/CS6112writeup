(******************************************************************************)
(* The Frenetic Project                                                       *)
(* frenetic@frenetic-lang.org                                                 *)
(******************************************************************************)
(* Copyright (C) 2011 Cornell University                                      *)
(*                                                                            *)
(* This program is free software: you can redistribute it and/or modify       *)
(* it under the terms of the GNU General Public License version 3 as          *)
(* published by the Free Software Foundation.                                 *)
(*                                                                            *)
(* This program is distributed in the hope that it will be useful,            *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(* GNU General Public License for more details.                               *)
(*                                                                            *)
(* You should have received a copy of the GNU General Public License          *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *)
(******************************************************************************)
(* /src/compiler/toplevel.ml                                                  *)
(* Actual Frenetic front-end                                                  *)
(* $Id$ *)
(******************************************************************************)

let sprintf = Printf.sprintf

let arg_spec = 
  [ ("-debug", Arg.String Prefs.add_debug_flag, ": print debugging information") ]

let usage prog = sprintf "Usage:\n    %s [options] F.fnet [F.fnet...]\n" prog

let anon_cell = ref []
let anon_arg x = anon_cell := (x::!anon_cell)

let go' prog () = 
  Arg.parse arg_spec anon_arg (usage prog);
  match !anon_cell with 
    | [fn] -> 
      begin 
        let _ = Lexer.setup fn in 
        let lexbuf = Lexing.from_string (Util.read fn) in       
        let _ = 
          try Parser.top Lexer.top lexbuf with 
            | Parsing.Parse_error ->
              (Error.error
                 (fun () -> Util.format "@[%s:@ syntax@ error@\n@]"
                   (Info.string_of_t (Lexer.info lexbuf)))) in 
        ()
      end
    | _ -> 
      begin 
        Util.format "@[%s@]" (usage prog); 
        exit 2  
      end
    
let go prog =
  try 
    Unix.handle_unix_error 
      (fun () -> Error.exit_if_error (go' prog))
      ();
    exit 0
  with e -> 
    Util.format "@[Uncaught exception %s@]" (Printexc.to_string e); 
    exit 2
