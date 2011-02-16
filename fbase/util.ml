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
(* /src/compiler/util.ml                                                      *)
(* Utility functions                                                          *)
(* $Id$ *)
(******************************************************************************)

let current_formatter = ref Format.std_formatter

let format f = Format.fprintf (!current_formatter) f

let format_to_string f =
  let old_formatter = !current_formatter in
  current_formatter := Format.str_formatter;
  f ();
  let s = Format.flush_str_formatter () in
  current_formatter := old_formatter;
  s
    
let flush () = 
  Format.pp_print_flush (!current_formatter) ()

let read_chan ch =
  let bs = in_channel_length ch in
  let s = String.create bs in
  really_input ch s 0 bs;
  s

let read file =
  if file = "-" then read_chan stdin
  else 
    let ch = open_in_bin file in
    try
      let s = read_chan ch in
      close_in ch; s
    with e -> close_in ch; raise e

let write file s =
  if file = "-" then output_string stdout s
  else 
    let ch = open_out_bin file in
    try
      output_string ch s; close_out ch
    with e ->
      close_out ch; raise e
