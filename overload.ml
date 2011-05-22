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
(* /src/compiler/overload.ml                                                  *)
(* Tools for overloaded operators                                             *)
(* $Id$ *)
(******************************************************************************)

include Syntax

(* Set to store overloaded operator possibilities *)
module OpSet = Set.Make (
  struct
    let compare = compare
    type t = typ list * Id.t
  end )

let extend_opset : OpSet.t -> OpSet.elt list -> OpSet.t =
  List.fold_left (fun s e -> OpSet.add e s)

let build_opset : OpSet.elt list -> OpSet.t =
  extend_opset OpSet.empty

let options = function
  | OSemi -> OpSet.empty
  | OEqual -> OpSet.empty
  | OMinus -> OpSet.empty
  | OLt -> OpSet.empty
  | OLeq -> OpSet.empty
  | OGt -> OpSet.empty
  | OGeq -> OpSet.empty
