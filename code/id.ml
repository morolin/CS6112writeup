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
(* /src/compiler/id.ml                                                        *)
(* Identifiers                                                                *)
(* $Id$ *)
(******************************************************************************)

(* type for identifiers *) 
type t = Info.t * string option * string
  
(* constructor *)
let mk i mo s = (i,mo,s)

(* accessors *)
let info_of_t (i,_,_) = i

let module_of_t (_,m,_) = m

let string_of_t (_,m,s) = 
  match m with 
    | None -> s
    | Some m' -> m' ^ "." ^ s
      
(* comparisons *)
let compare (_,mo1,s1) (_,mo2,s2) = 
  let cmp1 = compare mo1 mo2 in 
  if cmp1 <> 0 then cmp1
  else compare s1 s2

let equal x1 x2 = compare x1 x2 = 0

(* modifiers *)
let prime (i,mo,s) =
  (i,mo,s ^ "'")

(* constants *)
let wild = 
  (Info.M "_", None, "_")

type this_t = t

(* ordered type *)
module OrdId = struct
  type t = this_t
  let compare = compare
end

(* sets with identifiers as elements *)
module Set = Set.Make(OrdId)

(* maps with identifiers as keys *)
module Map = Map.Make(OrdId)

