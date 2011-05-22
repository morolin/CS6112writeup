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
(* /src/compiler/typechecker.ml                                               *)
(* Frenetic syntax type checking interface                                    *)
(* $Id$ *)
(******************************************************************************)

open Syntax

module ConstraintSet : Set.S with type elt = typ * typ

exception TypeException of (Info.t * string)

val unify : ConstraintSet.t -> typ Id.Map.t option
(** Unify constraints and produce Some substitution, or None if it failed **)

val unify_err : ConstraintSet.t -> typ Id.Map.t
(** Unify constraints and produce a substitution, or TypeException **)

val typecheck_exp :
  Id.t Util.zlist_t ref ->
  scheme Id.Map.t ->
  (Id.t * typ option) list Id.Map.t ->
  exp ->
  typ * ConstraintSet.t * exp
(** Try to typecheck an expression; if it succeeds, return the type,the
 ** constraint set generated, and the  expression with overloaded operators
 ** resolved, otherwise, raise a TypeException **)

val typecheck_decl :
  Id.t Util.zlist_t ref ->
  scheme Id.Map.t * (Id.t * typ option) list Id.Map.t * ConstraintSet.t ->
  decl ->
  scheme Id.Map.t * (Id.t * typ option) list Id.Map.t * ConstraintSet.t * decl

val typecheck_modl : modl -> modl 
(** Try to typecheck a module; if it succeeds, return the module with overloaded
 ** operators resolved, otherwise, raise a TypeException **)

