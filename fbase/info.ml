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
(* /src/compiler/fbase/info.ml                                                *)
(* Parsing information                                                        *)
(* $Id$ *)
(******************************************************************************)

type t = 
    I of string * (int * int) * (int * int) 
  | M of string

let string_of_t i = match i with 
  | I (fn, (l1,c1),(l2,c2)) -> 
      let f = "File \"" ^ fn ^ "\", " in
      if l2=l1
      then Printf.sprintf "%sline %d, characters %d-%d" f l1 c1 c2
      else Printf.sprintf "%sline %d, character %d, to line %d, character %d" f l1 c1 l2 c2
  | M s -> s

let imerge i1 i2 = match i1,i2 with
    | I(f,p1,_),I(_,_,p2) -> I(f,p1,p2)
    | I(_),_ -> i1
    | _ -> i2

let emerge i1 i2 = match i1,i2 with
    | I(f,_,p1),I(_,p2,_) -> I(f,p1,p2)
    | I(_),_ -> i1
    | _ -> i2
