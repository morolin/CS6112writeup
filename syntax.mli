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

(** {2 Frenetic Abstract Syntax} *)
type typ = 
    (* base types *)
    | TUnit                           (* unit *)
    | TBool                           (* booleans *)
    | TInteger                        (* integers *)
    | TChar                           (* chars *)
    | TString                         (* strings *)

    (* products and sums *)
    | TProduct of typ * typ           (* products *)
    | TData of typ list * Id.t        (* data types *)

    (* function types *)
    | TFunction of typ * typ 
    | TVar of Id.t                    (* variables *)

(** Type abstract syntax *)

and scheme = Scheme of Id.Set.t * typ 
(** Type schemes abstract syntax *)

and exp = 
  (* lambda calculus *)
  | EVar of Info.t * Id.t 
  | EApp of Info.t * exp * exp 
  | EFun of Info.t * param * exp 
  | ECond of Info.t * exp * exp * exp 
  | ELet of Info.t * bind * exp 
  | EAsc of Info.t * exp * typ 
  | EOver of Info.t * op * exp list
      
  (* with products, case *)
  | EPair of Info.t * exp * exp 
  | ECase of Info.t * exp * (pattern * exp) list 
      
  (* unit, ints, characters, strings, bools  *)
  | EUnit    of Info.t  
  | EInteger of Info.t * int    
  | EChar    of Info.t * char
  | EString  of Info.t * string
  | EBool of Info.t * bool 
(** Expression abstract syntax *)

and op =
  | OSemi 
  | OEqual
  | OMinus
  | OLt
  | OLeq
  | OGt
  | OGeq
(** Overloaded operator abstract syntax *)

and param = Param of Info.t * pattern * typ option
(** Parameter abstract syntax *)

and bind = Bind of Info.t * pattern * typ option * exp 
(** Binding abstract syntax *)

and pattern = 
  | PWild of Info.t
  | PUnit of Info.t
  | PBool of Info.t * bool
  | PInteger of Info.t * int
  | PString of Info.t * string
  | PVar of Info.t * Id.t * typ option
  | PData of Info.t * Id.t * pattern option 
  | PPair of Info.t * pattern * pattern
(** Pattern abstract syntax *)

type decl = 
  | DLet  of Info.t * bind 
  | DType of Info.t * Id.t list * Id.t * (Id.t * typ option) list 
(** Declaration abstract syntax *)

type modl = Modl of Info.t * Id.t * decl list
(** Module abstract syntax *)

module OpOptionSet : Set.S with type elt = (typ list * typ * Id.t)
(** Set to store overloaded operator possibilities **)

val (^>) : typ -> typ -> typ
(** [s1 ^> s2] is the function typ from [s1] to [s2]. *)

val (^*) : typ -> typ -> typ
(** [s1 ^* s2] is the product typ between [s1] and [s2]. *)

val info_of_exp : exp -> Info.t
(** [info_of_exp e] returns the parsing info associated to expression [e]. *)

val info_of_pattern : pattern -> Info.t
(** [info_of_pat p] returns the parsing info associated to pattern [p]. *)

val info_of_module : modl -> Info.t
(** [info_of_module m] returns the parsing info associated to module [m]. *)

val info_of_decl : decl -> Info.t
(** [info_of_decl d] returns the parsing info associated to declaration [d]. *)

val id_of_module : modl -> Id.t
(** [id_of_module m] returns the name of module [m]. *)

val pattern_of_param : param -> pattern
(** [pattern_of_param p] extracts a pattern from parameter [p]. *)

val pat_of_binding : bind -> pattern
(** [pat_of_binding b] returns the name of the variable bound in [b]. *)

val exp_of_binding : bind -> exp
(** [exp_op_binding p] returns the expression of binding [b]. *)

(* ------ constructors ----- *)
val mk_unit : Info.t -> exp 
val mk_int : Info.t -> int -> exp 
val mk_string : Info.t -> string -> exp 
val mk_var : Id.t -> exp
val mk_fun : Info.t -> Id.t -> typ option -> exp -> exp
val mk_multi_fun : Info.t -> param list -> exp -> exp
val mk_app : Info.t -> exp -> exp -> exp
val mk_app3 : Info.t -> exp -> exp -> exp -> exp
val mk_app4 : Info.t -> exp -> exp -> exp -> exp -> exp
val mk_over : Info.t -> op -> exp list -> exp
val mk_let : Info.t -> Id.t -> typ -> exp -> exp -> exp
val mk_asc : Info.t -> exp -> typ -> exp
val mk_if : Info.t -> exp -> exp -> exp -> exp 
val mk_bin_op : Info.t -> exp -> exp -> exp -> exp
val mk_tern_op : Info.t -> exp -> exp -> exp -> exp -> exp

val bv : pattern -> Id.Set.t
val ftv : typ -> Id.Set.t
val fv : exp -> Id.Set.t
val ftv_exp : exp -> Id.Set.t

val op_options : op -> OpOptionSet.t
