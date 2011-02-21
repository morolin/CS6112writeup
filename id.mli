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
(* /src/compiler/id.ml                                                        *)
(* Identifiers interface                                                      *)
(* $Id$ *)
(******************************************************************************)

type t = Info.t * string option * string
(** the type of (qualified) identifiers: a triple consisting of
    parsing information, an optional module, and the identifier
    itself *)
    
val mk : Info.t -> string option -> string -> t
(** [mk i mo s] constructs an identifier for [s] under optional module
    [mo] with parsing info [i]. *)

val info_of_t : t -> Info.t
(** [info_of_t x] returns the parsing info for [x]. *)

val module_of_t : t -> string option 
(** [module_of_t x] returns the optional module represented by [x]. *)

val string_of_t : t -> string 
(** [string_of_t x] returns the string that [x] represents. *)
  
val prime : t -> t 
(** [prime x] returns [x']. *)
  
val compare : t -> t -> int
(** [compare x y] compares [x] and [y], ignoring parsing info. *)

val equal : t -> t -> bool
(** [equal x y] returns [true] iff [x] and [y] represent the same
    string. *)

val wild : t
(** [wild] is a constant representing the "don't care" string "_" *)

module Set : Set.S with type elt = t
(** Sets with identifiers as elements *)

module Map : Map.S with type key = t
(** Maps with identifiers as keys *)
