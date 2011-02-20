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
(* /src/compiler/fbase/util.ml                                                *)
(* Utility functions interface                                                *)
(* $Id$ *)
(******************************************************************************)

(** {2 Formatting} *)

val format : ('a, Format.formatter, unit) format -> 'a
(** Formats using the current formatting channel. *)

val flush : unit -> unit 
(** Flushes the current formatting channel. *)

val format_to_string : (unit -> unit) -> string
(** [format_to_string f] runs [f], redirecting formatting functions to
    the string returned as a result. *)

val concat_list : string -> string list -> string
  (** [concat_list sep l] concatenates a string list using [sep] to
      separate elements *)

val format_list : (unit, Format.formatter, unit) format -> ('a -> unit) -> 'a list -> unit
  (** [format_list sep f l] formats [l] using [f] to format elements and
      calls [sep] between elements *)

(** {2 I/O} *)

val read : string -> string
(** [read f] reads [f] (or standard input if [f] is ["-"]) and returns
    its contents. *)

val write : string -> string -> unit
(** [write f s] opens [f] (or standard output if [f] is ["-"]) and 
    overwrites its contents with [s]. If [f] does not exist it is 
    created. *)
