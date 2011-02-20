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
(* /src/compiler/id.ml                                                        *)
(* Identifiers                                                                *)
(* $Id$ *)
(******************************************************************************)

(* type for identifiers *) 
type t = Info.t * string option * string
  
(* constructor *)
let mk i mo s = (i,mo,s)

(* accessors *)
let info_of_t (i,_,_) = i

let module_of_t (_,m,_) = m

let string_of_t (_,_,s) = s
      
(* comparisons *)
let compare (_,mo1,s1) (_,mo2,s2) = 
  let cmp1 = compare mo1 mo2 in 
  if cmp1 <> 0 then cmp1
  else compare s1 s2

let equal x1 x2 = compare x1 x2 = 0

(* modifiers *)
let prime (i,mo,s) =
  (i,mo,s ^ "'")

(* constants *)
let wild = 
  (Info.M "_", None, "_")

type this_t = t

(* ordered type *)
module OrdId = struct
  type t = this_t
  let compare = compare
end

(* sets with identifiers as elements *)
module Set = Set.Make(OrdId)

(* maps with identifiers as keys *)
module Map = Map.Make(OrdId)

