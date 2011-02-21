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

let concat fold sep is_empty empty pretty structure = 
  fold 
    (fun acc h -> 
       if is_empty acc then pretty h
       else sep acc (pretty h))
    empty 
    structure 

let concat_list sep l = 
  concat
    Data.List.fold_left
    (fun x y -> Printf.sprintf "%s%s%s" x sep y)
    (fun x -> String.length x = 0)
    ""
    (fun x -> x)
    l

let format_list sep f l =
  let extract_thk = function 
    | Some thk -> thk
    | None -> (fun () -> ()) in
  let thko =
    concat
      Data.List.fold_left
      (fun x y -> 
         Some (fun () -> 
                 extract_thk x ();
                 format sep;
                 extract_thk y ()))
      (fun x -> x = None)
      None
      (fun x -> Some (fun () -> f x))
      l in 
    extract_thk thko ()
