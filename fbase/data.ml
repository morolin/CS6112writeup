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
(* /src/compiler/fbase/data.ml                                                *)
(* Error                                                                      *)
(* $Id$ *)
(******************************************************************************)

module OrdInt = struct
  type t = int
  let compare = Pervasives.compare
end
module IntSet = Set.Make(OrdInt)

module OrdString = struct
  type t = string
  let compare = Pervasives.compare
end
module StringSet = Set.Make(OrdString)

module L = List
module List = struct
  let length = L.length
  let hd = L.hd
  let tl = L.tl
  let nth = L.nth
  let rev = L.rev
  let append = L.append
  let rev_append = L.rev_append
  let concat = L.concat
  let flatten = L.flatten
  let iter = L.iter
  let map = L.map
  let rev_map = L.rev_map
  let fold_left = L.fold_left
  let fold_right = L.fold_right
  let iter2 = L.iter2
  let map2 = L.map2
  let rev_map2 = L.rev_map2
  let fold_left2 = L.fold_left2
  let fold_right2 = L.fold_right2
  let for_all = L.for_all
  let exists = L.exists
  let for_all2 = L.for_all2
  let exists2 = L.exists2
  let mem = L.mem
  let memq = L.memq
  let find = L.find
  let filter = L.filter
  let find_all = L.find_all
  let partition = L.partition
  let assoc = L.assoc
  let assq = L.assq
  let mem_assoc = L.mem_assoc
  let mem_assq = L.mem_assq
  let remove_assoc = L.remove_assoc
  let remove_assq = L.remove_assq
  let split = L.split
  let combine = L.combine
  let sort = L.sort
  let stable_sort = L.stable_sort
  let fast_sort = L.fast_sort
  let merge = L.merge
end
