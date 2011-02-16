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
