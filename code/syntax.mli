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
(* /src/compiler/syntax.ml                                                    *)
(* Frenetic abstract syntax interface                                         *)
(* $Id$ *)
(******************************************************************************)

module StrSet : Set.S with type elt = string

type variable =
  | VVar of Info.t * string
  | VAck of Info.t * string
  | VTrue of Info.t * string
  | VFalse of Info.t * string

type boolean = 
  | BVar of Info.t * variable
  | BLit of Info.t * bool
  | BProbeRecv of Info.t * string
  | BProbeSend of Info.t * string
  | BAnd of Info.t * boolean * boolean
  | BOr of Info.t * boolean * boolean
  | BNot of Info.t * boolean

type channel =
  | CSend of Info.t * string * boolean
  | CRecv of Info.t * string * variable
  | CBullet of Info.t * channel * channel

type chp =
  | PGets of Info.t * variable * boolean
  | PSelect of Info.t * select
  | PLoop of Info.t * select
  | PChannel of Info.t * channel
  | PSeq of Info.t * chp * chp
  | PPar of Info.t * chp * chp
  | PSkip of Info.t

and select =
  | SDet of Info.t * select_det
  | SNonDet of Info.t * select_nondet

and select_det = 
  | SDBase of Info.t * boolean * chp
  | SDRecur of Info.t * boolean * chp * select_det

and select_nondet = 
  | SNBase of Info.t * boolean * chp
  | SNRecur of Info.t * boolean * chp * select_nondet

type hse =
  | HGets of Info.t * variable * boolean
  | HSelect of Info.t * hselect
  | HLoop of Info.t * hselect
  | HSeq of Info.t * hse * hse
  | HPar of Info.t * hse * hse
  | HSkip of Info.t

and hselect =
  | HSDet of Info.t * hselect_det
  | HSNonDet of Info.t * hselect_nondet

and hselect_det = 
  | HSDBase of Info.t * boolean * hse
  | HSDRecur of Info.t * boolean * hse * hselect_det

and hselect_nondet = 
  | HSNBase of Info.t * boolean * hse
  | HSNRecur of Info.t * boolean * hse * hselect_nondet

val chp_of_hse : hse -> chp

val info_of_boolean : boolean -> Info.t
(** [info_of_boolean e] returns the parsing info associated to expression [e]. *)
val info_of_variable : variable -> Info.t
(** [info_of_boolean e] returns the parsing info associated to expression [e]. *)
val info_of_chp : chp -> Info.t
(** [info_of_boolean e] returns the parsing info associated to expression [e]. *)
val info_of_select : select -> Info.t
(** [info_of_boolean e] returns the parsing info associated to expression [e]. *)
val info_of_select_det : select_det -> Info.t
(** [info_of_boolean e] returns the parsing info associated to expression [e]. *)
val info_of_select_nondet : select_nondet -> Info.t
(** [info_of_boolean e] returns the parsing info associated to expression [e]. *)
val info_of_channel : channel -> Info.t
(** [info_of_boolean e] returns the parsing info associated to expression [e]. *)

val info_of_hse : hse -> Info.t
val info_of_hselect : hselect -> Info.t
val info_of_hselect_det : hselect_det -> Info.t
val info_of_hselect_nondet : hselect_nondet -> Info.t
