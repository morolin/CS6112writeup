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
(* /src/compiler/fbase/info.mli                                               *)
(* Parsing information interface                                              *)
(* $Id$ *)
(******************************************************************************)

type t = I of string * (int * int) * (int * int) | M of string
(** [t] represents a location that spans a chunk of a file, or a message if 
    no precise location is available. *)

val string_of_t : t -> string 
(** [string_of_t] pretty prints a location for easy parsing Emacs
    [compile-mode]. *)

val imerge : t -> t -> t
(** [imerge i1 i2] merges the locations [i1] and [i2] into a new
    location including the endpoints. *)

val emerge : t -> t -> t
(** [emerge i1 i2] merges the locations [i1] and [i2] into a new
    location excluding the endpoints. *)
