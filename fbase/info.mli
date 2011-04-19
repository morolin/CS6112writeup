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

val dummy : string -> t
