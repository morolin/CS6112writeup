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
