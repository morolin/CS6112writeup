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

val format_boolean : boolean -> unit
(** [format_boolean b] pretty prints [b] using [Util.format]. *)
val format_variable : variable -> unit
(** [format_variable v] pretty prints [v] using [Util.format]. *)
val format_program : program -> unit
(** [format_program p] pretty prints [p] using [Util.format]. *)
val format_select : select -> unit
(** [format_select s] pretty prints [s] using [Util.format]. *)
val format_select_det : select_det -> unit
(** [format_select_det s] pretty prints [s] using [Util.format]. *)
val format_select_nondet : select_nondet -> unit
(** [format_select_nondet s] pretty prints [s] using [Util.format]. *)
val format_channel : channel -> unit
(** [format_channel c] pretty prints [c] using [Util.format]. *)

(** {2 Pretty printing to strings} *)

val string_of_boolean : boolean -> string
(** [string_of_boolean b] pretty prints [b] to a string. *)

val string_of_variable : variable -> string
(** [string_of_variable v] pretty prints [v] to a string. *)

val string_of_program : program -> string
(** [string_of_program p] pretty prints [p] to a string. *)

val string_of_select : select -> string
(** [string_of_select s] pretty prints [s] to a string. *)

val string_of_select_det : select_det -> string
(** [string_of_select_det s] pretty prints [s] to a string. *)

val string_of_select_nondet : select_nondet -> string
(** [string_of_select_nondet s] pretty prints [s] to a string. *)

val string_of_channel : channel -> string
(** [string_of_channel c] pretty prints [c] to a string. *)
