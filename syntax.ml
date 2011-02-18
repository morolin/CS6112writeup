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
(* Frenetic abstract syntax                                                   *)
(* $Id$ *)
(******************************************************************************)

let (@) = Data.List.append 

(* types *)
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

and scheme = Scheme of Id.Set.t * typ 

(* parameters *)
and param = Param of Info.t * Id.t * typ

(* variable bindings *)
and bind = Bind of Info.t * pat * typ option * exp 

(* expressions *)
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

(* patterns *)
and pat = 
  | PWld of Info.t
  | PUnt of Info.t
  | PBol of Info.t * bool
  | PInt of Info.t * int
  | PStr of Info.t * string
  | PVar of Info.t * Id.t * typ option
  | PVnt of Info.t * Id.t * pat option 
  | PPar of Info.t * pat * pat

(* declarations *)
type decl = 
  | DLet  of Info.t * bind 
  | DType of Info.t * Id.t list * Id.t * (Id.t * typ option) list 

(* modules *)
type modl = Modl of Info.t * Id.t * decl list

(* infix constructors for functions and products *)
let (^>) s1 s2 = TFunction(s1,s2)
let (^*) s1 s2 = TProduct(s1,s2)

(* ----- accessor functions ----- *)
let typ_of_param p0 = match p0 with
  | Param(_,_,t) -> t

let id_of_param p0 = match p0 with
  | Param(_,x,_) -> x

let pat_of_binding b0 = match b0 with 
  | Bind(_,p,_,_) -> p

let exp_of_binding b0 = match b0 with 
  | Bind(_,_,_,e) -> e

let rec info_of_exp e = match e with 
  | EApp(i,_,_) -> i
  | EVar(i,_) -> i
  | EFun(i,_,_,_) -> i
  | ELet(i,_,_) -> i 
  | EPair(i,_,_) -> i
  | ECase(i,_,_) -> i
  | EUnit(i) -> i
  | EBool(i,_) -> i
  | EInteger(i,_) -> i    
  | EChar(i,_) -> i 
  | EString(i,_) -> i

let info_of_pat = function
  | PWld(i) -> i
  | PUnt(i) -> i
  | PBol(i,_) -> i
  | PInt(i,_) -> i
  | PStr(i,_) -> i
  | PVar(i,_,_) -> i 
  | PVnt(i,_,_) -> i
  | PPar(i,_,_) -> i

let info_of_module = function
  | Modl(i,_,_) -> i

let id_of_module = function
  | Modl(_,x,_) -> x

(* constructors *)
let mk_unit i = 
  EUnit(i)

let mk_int i n = 
  EInteger(i,n)

let mk_string i s = 
  EString(i,s)

let mk_var x = 
  EVar(Id.info_of_t x,x)

let mk_app i e1 e2 = 
  EApp(i,e1,e2)

let mk_app3 i e1 e2 e3 = 
  mk_app i (mk_app i e1 e2) e3

let mk_app4 i e1 e2 e3 e4 =
  mk_app i (mk_app i e1 e2) (mk_app i e3 e4)

let mk_let i x s1 e1 e2 =
  let b = Bind(i,PVar(i,x,Some s1),None,e1) in 
  ELet(i,b,e2)

let mk_fun i x s e1 =
  let p = Param(i,x,s) in  
  EFun(i,p,None,e1)

let mk_if i e0 e1 e2 =
  let bs = [(PBol(i,true),e1);(PBol(i,false),e2)] in 
  ECase(i,e0,bs)

let mk_app i e1 e2 = 
  EApp(i,e1,e2)

let mk_bin_op i o e1 e2 = 
  mk_app i (mk_app i o e1) e2

let mk_tern_op i o e1 e2 e3 = 
  mk_app i (mk_bin_op i o e1 e2) e3
