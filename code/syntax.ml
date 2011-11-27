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
(* Frenetic abstract syntax                                                   *)
(* $Id$ *)
(******************************************************************************)
module StrSet = Set.Make(String)

exception UnimplementedException
type boolean = 
  | BVar of Info.t * variable
  | BLit of Info.t * bool
  | BProbeRecv of Info.t * string
  | BProbeSend of Info.t * string
  | BAnd of Info.t * boolean * boolean
  | BOr of Info.t * boolean * boolean
  | BNot of Info.t * boolean

and variable =
  | VVar of Info.t * string
  | VAck of Info.t * string
  | VTrue of Info.t * string
  | VFalse of Info.t * string

and program =
  | PGets of Info.t * variable * boolean
  | PSelect of Info.t * select
  | PLoop of Info.t * select
  | PChannel of Info.t * channel
  | PSeq of Info.t * program * program
  | PPar of Info.t * program * program
  | PSkip of Info.t

and select =
  | SDet of Info.t * select_det
  | SNonDet of Info.t * select_nondet

and select_det = 
  | SDBase of Info.t * boolean * program
  | SDRecur of Info.t * boolean * program * select_det

and select_nondet = 
  | SNBase of Info.t * boolean * program
  | SNRecur of Info.t * boolean * program * select_nondet

and channel =
  | CSend of Info.t * string * boolean
  | CRecv of Info.t * string * variable
  | CBullet of Info.t * channel * channel

let rec info_of_boolean b = match b with
  | BVar(i,_) -> i
  | BLit(i,_) -> i
  | BProbeRecv(i,_) -> i
  | BProbeSend(i,_) -> i
  | BAnd(i,_,_) -> i
  | BOr(i,_,_) -> i
  | BNot(i,_) -> i

let rec info_of_variable v = match v with
  | VVar(i,_) -> i
  | VAck(i,_) -> i
  | VTrue(i,_) -> i
  | VFalse(i,_) -> i

let rec info_of_program p = match p with
  | PGets(i,_,_) -> i
  | PSelect(i,_) -> i
  | PLoop(i,_) -> i
  | PChannel(i,_) -> i
  | PSeq(i,_,_) -> i
  | PPar(i,_,_) -> i
  | PSkip(i) -> i

let rec info_of_select s = match s with
  | SDet(i,_) -> i
  | SNonDet(i,_) -> i

let rec info_of_select_det s = match s with
  | SDBase(i,_,_) -> i
  | SDRecur(i,_,_,_) -> i

let rec info_of_select_nondet s = match s with
  | SNBase(i,_,_) -> i
  | SNRecur(i,_,_,_) -> i

let rec info_of_channel c = match c with
  | CSend(i,_,_) -> i
  | CRecv(i,_,_) -> i
  | CBullet(i,_,_) -> i
