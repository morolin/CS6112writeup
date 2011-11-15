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

let dummy s = M(s)

let imerge i1 i2 = match i1,i2 with
    | I(f,p1,_),I(_,_,p2) -> I(f,p1,p2)
    | I(_),_ -> i1
    | _ -> i2

let emerge i1 i2 = match i1,i2 with
    | I(f,_,p1),I(_,p2,_) -> I(f,p1,p2)
    | I(_),_ -> i1
    | _ -> i2
