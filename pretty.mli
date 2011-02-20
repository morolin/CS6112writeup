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
(* /src/compiler/pretty.mli                                                   *)
(* Pretty printer interface                                                   *)
(* $Id$ *)
(******************************************************************************)

open Syntax

(** {2 Pretty printing using formatting functions } *)

val format_type : typ -> unit 
  (** [format_type s] pretty prints [s] using [Util.format]. *)

val format_pattern : pattern -> unit
(** [format_pat p] pretty prints [p] using [Util.format]. *)

val format_param : param -> unit
(** [format_param p] pretty prints [p] using [Util.format]. *)

val format_bind : bind -> unit
(** [format_bind b] pretty prints [b] using [Util.format]. *)

val format_exp : exp -> unit
(** [format_exp e] pretty prints [e] using [Util.format]. *)

val format_op : op -> unit
(** [format_op o] pretty prints [o] using [Util.format]. *)

val format_decl : decl -> unit
(** [format_decl d] pretty prints [d] using [Util.format]. *)

val format_modl : modl -> unit
(** [format_modl m] pretty prints [m] using [Util.format]. *)

(** {2 Pretty printing to strings} *)

val string_of_type : typ -> string
(** [string_of_type s] pretty prints [s] to a string. *)

val string_of_pattern : pattern -> string
(** [string_of_pat p] pretty prints [p] to a string. *)

val string_of_param : param -> string
(** [string_of_param p] pretty prints [p] to a string. *)

val string_of_bind : bind -> string
(** [string_of_bind b] pretty prints [b] to a string. *)

val string_of_exp : exp -> string
(** [string_of_exp e] pretty prints [e] to a string. *)

val string_of_op : op -> string
(** [string_of_op o] pretty prints [o] to a string. *)

val string_of_decl : decl -> string
(** [string_of_decl d] pretty prints [d] to a string. *)

val string_of_modl : modl -> string
(** [string_of_modl m] pretty prints [m] to a string. *)
