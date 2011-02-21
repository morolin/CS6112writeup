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
