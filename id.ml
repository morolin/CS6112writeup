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
type t = Info.t * string 
  
(* constructor *)
let mk i s = (i,s)

(* accessors *)
let info_of_t (i,_) = i
let string_of_t (_,s) = s
  
(* comparisons *)
let compare ((_,x1):t) ((_,x2):t) = compare x1 x2
let equal ((_,x1):t) ((_,x2):t) = x1 = x2

(* modifiers *)
let prime (i,x) = (i,x ^ "'")

(* constants *)
let wild = (Info.M "_", "_")

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

