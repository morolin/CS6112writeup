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
(* Identifiers interface                                                      *)
(* $Id$ *)
(******************************************************************************)

type t = Info.t * string
(** the type of identifiers: a pair of parsing information and a string *)
    
val mk : Info.t -> string -> t
(** [mk i s] constructs an identifier for [s] with parsing info [i]. *)

val info_of_t : t -> Info.t
(** [info_of_t x] returns the parsing info for [x]. *)

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
