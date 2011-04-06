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

open BatOption

exception UnimplementedException

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

(* expressions *)
and exp = 
  (* lambda calculus *)
  | EVar of Info.t * Id.t 
  | EApp of Info.t * exp * exp 
  | EFun of Info.t * param * exp 
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

(* overloaded operators *)
and op =
  | OSemi 
  | OEqual
  | OMinus
  | OLt
  | OLeq
  | OGt
  | OGeq

(* parameters *)
and param = Param of Info.t * pattern * typ option

(* variable bindings *)
and bind = Bind of Info.t * pattern * typ option * exp 

(* patterns *)
and pattern = 
  | PWild of Info.t
  | PUnit of Info.t
  | PBool of Info.t * bool
  | PInteger of Info.t * int
  | PString of Info.t * string
  | PVar of Info.t * Id.t * typ option
  | PData of Info.t * Id.t * pattern option 
  | PPair of Info.t * pattern * pattern

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
let pattern_of_param p0 = match p0 with
  | Param(_,x,_) -> x

let typ_of_param p0 = match p0 with
  | Param(_,_,t) -> t

let pat_of_binding b0 = match b0 with 
  | Bind(_,p,_,_) -> p

let exp_of_binding b0 = match b0 with 
  | Bind(_,_,_,e) -> e

let rec info_of_exp e = match e with 
  | EApp(i,_,_) -> i
  | EOver(i,_,_) -> i
  | EVar(i,_) -> i
  | EFun(i,_,_) -> i
  | ELet(i,_,_) -> i 
  | EAsc(i,_,_) -> i
  | EPair(i,_,_) -> i
  | ECase(i,_,_) -> i
  | EUnit(i) -> i
  | EBool(i,_) -> i
  | EInteger(i,_) -> i    
  | EChar(i,_) -> i 
  | EString(i,_) -> i

let info_of_pattern = function
  | PWild(i) -> i
  | PUnit(i) -> i
  | PBool(i,_) -> i
  | PInteger(i,_) -> i
  | PString(i,_) -> i
  | PVar(i,_,_) -> i 
  | PData(i,_,_) -> i
  | PPair(i,_,_) -> i

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

let mk_over i o l = 
  EOver(i,o,l)

let mk_app i e1 e2 = 
  EApp(i,e1,e2)

let mk_app3 i e1 e2 e3 = 
  mk_app i (mk_app i e1 e2) e3

let mk_app4 i e1 e2 e3 e4 =
  mk_app i (mk_app i e1 e2) (mk_app i e3 e4)

let mk_let i x s1 e1 e2 =
  let b = Bind(i,PVar(i,x,Some s1),None,e1) in 
  ELet(i,b,e2)

let mk_asc i e t =
  EAsc(i,e,t)

let mk_fun i x tyo e =
  let p = Param(i,PVar(i,x,tyo),tyo) in  
  EFun(i,p,e)

let mk_multi_fun i ps e = 
  Data.List.fold_right
    (fun p f -> EFun(i,p,f))
    ps e 

let mk_if i e0 e1 e2 =
  let bs = [(PBool(i,true),e1);(PBool(i,false),e2)] in 
  ECase(i,e0,bs)

let mk_app i e1 e2 = 
  EApp(i,e1,e2)

let mk_bin_op i o e1 e2 = 
  mk_app i (mk_app i o e1) e2

let mk_tern_op i o e1 e2 e3 = 
  mk_app i (mk_bin_op i o e1 e2) e3

let rec bound_v pat = match pat with
  | PWild(_)      -> StrSet.empty
  | PUnit(_)      -> StrSet.empty
  | PBool(_,_)    -> StrSet.empty
  | PInteger(_,_) -> StrSet.empty
  | PString(_,_)  -> StrSet.empty
  | PVar(_,(_,m,name),_) -> StrSet.singleton name
  | PData(_,_,pat_opt) -> BatOption.map_default bound_v StrSet.empty pat_opt
  | PPair(_,p1,p2) -> StrSet.union (bound_v p1) (bound_v p2)

let rec fv exp = match exp with
  | EVar(_,(_, m, name)) -> StrSet.singleton name
  | EApp (_, e1, e2) -> StrSet.union (fv e1) (fv e2)
  | EFun (_, Param(_,pat,_), e) -> StrSet.diff (fv e) (bound_v pat)
  | ELet (_, Bind(_,pat, _, e_bind), e) ->
    StrSet.union (fv e_bind) (StrSet.diff (fv e) (bound_v pat))
  | EAsc (_, exp, typ) -> fv exp
  | EOver (_,_,_) -> raise UnimplementedException
  
  | EPair (_, e1, e2) -> StrSet.union (fv e1) (fv e2)
  | ECase (_, m_exp, exps) ->
    StrSet.union
        (fv m_exp)
        (List.fold_left
            (fun acc (pat, e) ->
                StrSet.union (StrSet.diff (fv e) (bound_v pat)) acc)
            StrSet.empty exps)
  
  | EUnit(_)      -> StrSet.empty
  | EInteger(_,_) -> StrSet.empty
  | EChar(_,_)    -> StrSet.empty
  | EString(_,_)  -> StrSet.empty
  | EBool(_,_)    -> StrSet.empty

let rec pattern_ftv pat = match pat with
  | PWild(_)      -> StrSet.empty
  | PUnit(_)      -> StrSet.empty
  | PBool(_,_)    -> StrSet.empty
  | PInteger(_,_) -> StrSet.empty
  | PString(_,_)  -> StrSet.empty
  | PVar(_,_,t_opt) -> BatOption.map_default type_ftv StrSet.empty t_opt
  | PData(_,_,pat_opt) -> BatOption.map_default pattern_ftv StrSet.empty pat_opt
  | PPair(_,p1,p2) -> StrSet.union (pattern_ftv p1) (pattern_ftv p2)

and type_ftv typ = match typ with
  | TUnit -> StrSet.empty
  | TBool -> StrSet.empty
  | TInteger -> StrSet.empty
  | TChar -> StrSet.empty
  | TString -> StrSet.empty

  | TProduct(t1, t2) -> StrSet.union (type_ftv t1) (type_ftv t2)
  | TData(ts, _) ->
    List.fold_left
        (fun set t -> StrSet.union set (type_ftv t))
        StrSet.empty
        ts

  | TFunction(t1, t2) -> StrSet.union (type_ftv t1) (type_ftv t2)
  | TVar((_,_,name)) -> StrSet.singleton name

let rec ftv exp = match exp with
  | EVar(_,_) -> StrSet.empty
  | EApp (_, e1, e2) -> StrSet.union (ftv e1) (ftv e2)
  | EFun (_,Param(_,pat,t_opt),e) ->
    StrSet.union
    (ftv e)
    (StrSet.union
        (pattern_ftv pat)
        (BatOption.map_default type_ftv StrSet.empty t_opt)
    )
  | ELet (_,Bind(_,pat,t_opt,bind_e),e) ->
    StrSet.union
        (StrSet.union
            (pattern_ftv pat)
            (BatOption.map_default type_ftv StrSet.empty t_opt))
        (StrSet.union
            (ftv bind_e)
            (ftv e))
  | EAsc (_,e,t) -> StrSet.union (ftv e) (type_ftv t)
  | EOver (_,_,_) -> raise UnimplementedException

  | EPair (_,e1,e2) -> StrSet.union (ftv e1) (ftv e2)
  | ECase (_, m_exp, exps) ->
    StrSet.union
        (ftv m_exp)
        (List.fold_left
            (fun acc (pat, e) ->
                StrSet.union (StrSet.union (pattern_ftv pat) (ftv e)) acc)
            StrSet.empty exps)

  | EUnit (_)      -> StrSet.empty
  | EInteger (_,_) -> StrSet.empty
  | EChar (_,_)    -> StrSet.empty
  | EString (_,_)  -> StrSet.empty
  | EBool (_,_)    -> StrSet.empty
