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
(* /src/compiler/syntax.ml                                                    *)
(* Frenetic abstract syntax interface                                         *)
(* $Id$ *)
(******************************************************************************)

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

and param = Param of Info.t * Id.t * typ
(** Parameter abstract syntax *)

and bind = Bind of Info.t * pat * typ option * exp 
(** Binding abstract syntax *)

and exp = 
    (* lambda calculus *)
    | EApp  of Info.t * exp * exp 
    | EVar  of Info.t * Id.t 
    | EFun  of Info.t * param * typ option * exp 
    | ELet  of Info.t * bind * exp 

    (* with products, case *)
    | EPair of Info.t * exp * exp 
    | ECase of Info.t * exp * (pat * exp) list 

    (* unit, ints, characters, strings, bools  *)
    | EUnit    of Info.t  
    | EInteger of Info.t * int    
    | EChar    of Info.t * char
    | EString  of Info.t * string
    | EBool of Info.t * bool 
(** Expression abstract syntax *)

and pat = 
  | PWld of Info.t
  | PUnt of Info.t
  | PBol of Info.t * bool
  | PInt of Info.t * int
  | PStr of Info.t * string
  | PVar of Info.t * Id.t * typ option
  | PVnt of Info.t * Id.t * pat option 
  | PPar of Info.t * pat * pat
(** Pattern abstract syntax *)

type decl = 
  | DLet  of Info.t * bind 
  | DType of Info.t * Id.t list * Id.t * (Id.t * typ option) list 
(** Declaration abstract syntax *)

type modl = Modl of Info.t * Id.t * decl list
(** Module abstract syntax *)

val (^>) : typ -> typ -> typ
(** [s1 ^> s2] is the function typ from [s1] to [s2]. *)

val (^*) : typ -> typ -> typ
(** [s1 ^* s2] is the product typ between [s1] and [s2]. *)

val info_of_exp : exp -> Info.t
(** [info_of_exp e] returns the parsing info associated to expression [e]. *)

val info_of_pat : pat -> Info.t
(** [info_of_pat p] returns the parsing info associated to pattern [p]. *)

val info_of_module : modl -> Info.t
(** [info_of_module m] returns the parsing info associated to module [m]. *)

val id_of_module : modl -> Id.t
(** [id_of_module m] returns the name of module [m]. *)

val typ_of_param : param -> typ
(** [typ_of_param p] returns the typ declared with parameter [p]. *)

val id_of_param : param -> Id.t
(** [typ_of_param p] returns the name of parameter [p]. *)

val pat_of_binding : bind -> pat
(** [pat_of_binding b] returns the name of the variable bound in [b]. *)

val exp_of_binding : bind -> exp
(** [exp_op_binding p] returns the expression of binding [b]. *)

(* ------ constructors ----- *)
val mk_unit : Info.t -> exp 
val mk_int : Info.t -> int -> exp 
val mk_string : Info.t -> string -> exp 
val mk_var : Id.t -> exp
val mk_fun : Info.t -> Id.t -> typ -> exp -> exp
val mk_app : Info.t -> exp -> exp -> exp
val mk_app3 : Info.t -> exp -> exp -> exp -> exp
val mk_app4 : Info.t -> exp -> exp -> exp -> exp -> exp
val mk_let : Info.t -> Id.t -> typ -> exp -> exp -> exp
val mk_if : Info.t -> exp -> exp -> exp -> exp 
val mk_bin_op : Info.t -> exp -> exp -> exp -> exp
val mk_tern_op : Info.t -> exp -> exp -> exp -> exp -> exp
