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

(* Set to store overloaded operator possibilities *)
module OpOptionSet = Set.Make (
  struct
    let compare = compare
    type t = typ list * typ * Id.t
  end )

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
  | ECond(i,_,_,_) -> i
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

let info_of_decl = function
  | DLet(i,_) -> i
  | DType(i,_,_,_) -> i

let info_of_module = function
  | Modl(i,_,_) -> i

let id_of_module = function
  | Modl(_,x,_) -> x

let rec vars_of_pattern p = match p with
  | PWild(_) -> StrSet.empty
  | PUnit(_) -> StrSet.empty
  | PBool(_) -> StrSet.empty
  | PInteger(_) -> StrSet.empty
  | PString(_) -> StrSet.empty
  | PVar(_,(_,_,x),_) -> StrSet.singleton x
  | PData(_,_,po) -> 
    begin match po with 
      | None -> StrSet.empty
      | Some p1 -> vars_of_pattern p1
    end
  | PPair(_, p1, p2) ->
    StrSet.union (vars_of_pattern p1) (vars_of_pattern p2)

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

(* bound/free variables *)

let rec bv p = match p with
  | PWild(_)      -> Id.Set.empty
  | PUnit(_)      -> Id.Set.empty
  | PBool(_,_)    -> Id.Set.empty
  | PInteger(_,_) -> Id.Set.empty
  | PString(_,_)  -> Id.Set.empty
  | PVar(_,id,_) -> Id.Set.singleton id 
  | PData(_,_,po) -> BatOption.map_default bv Id.Set.empty po
  | PPair(_,p1,p2) -> Id.Set.union (bv p1) (bv p2)

let rec fv exp = match exp with
  | EVar(_,id) -> Id.Set.singleton id 
  | EApp (_, e1, e2) -> Id.Set.union (fv e1) (fv e2)
  | EFun (_, Param(_,pat,_), e) -> Id.Set.diff (fv e) (bv pat)
  | ECond (_, e1,e2,e3) -> Id.Set.union (Id.Set.union (fv e1) (fv e2)) (fv e3)
  | ELet (_, Bind(_,pat, _, e_bind), e) ->
    Id.Set.union (fv e_bind) (Id.Set.diff (fv e) (bv pat))
  | EAsc (_, exp, typ) -> fv exp
  | EOver (_,_,_) -> raise UnimplementedException
  
  | EPair (_, e1, e2) -> Id.Set.union (fv e1) (fv e2)
  | ECase (_, m_exp, exps) ->
    Id.Set.union
        (fv m_exp)
        (List.fold_left
            (fun acc (p, e) ->
                Id.Set.union (Id.Set.diff (fv e) (bv p)) acc)
            Id.Set.empty exps)
  
  | EUnit(_)      -> Id.Set.empty
  | EInteger(_,_) -> Id.Set.empty
  | EChar(_,_)    -> Id.Set.empty
  | EString(_,_)  -> Id.Set.empty
  | EBool(_,_)    -> Id.Set.empty

(* fresh variables *)
let fresh xs = 
  let rec aux i = 
    let y = 
      (String.make 1 (Char.chr (97 + i mod 26))) ^
      (if i > 25 then String.make (i - 25) '\'' else "") in 
    if StrSet.mem y xs then aux (succ i)
    else y in 
  aux 0 

let fresh_id xs = 
  let rec aux i = 
    let y = 
      (Info.dummy(""),None,
      (String.make 1 (Char.chr (97 + i mod 26))) ^
      (if i > 25 then String.make (i - 25) '\'' else "")) in
    if Id.Set.mem y xs then aux (succ i)
    else y in 
  aux 0 

(* substitution *)

let rec subst_pattern p (x:Id.t) p0 = match p0 with 
  | PWild(_) -> p0
  | PUnit(_) -> p0
  | PBool(_) -> p0
  | PInteger(_) -> p0
  | PString(_) -> p0
  | PVar(i1,y,t) -> 
    if Id.equal x y then p
    else p0
  | PData(i,c,po) -> 
    let po' = match po with 
      | None -> None
      | Some p1 -> Some (subst_pattern p x p1) in 
    PData(i,c,po')
  | PPair(i,p1,p2) -> 
    PPair(i,subst_pattern p x p1, subst_pattern p x p2)

let rec subst_exp e x e0 = 
  let freshen p e1 = 
    let e_fv = fv e in 
    Id.Set.fold 
      (fun y (pi,e1i) -> 
        let z = fresh_id (Id.Set.inter (bv pi) e_fv) in 
        let z_info = Info.M "fresh variable" in
        let z_pat = PVar(z_info,z,None) in 
        let z_var = EVar(z_info,z) in 
        let pi' = subst_pattern z_pat y pi in 
        let ei' = subst_exp z_var y e1i in 
        (pi',ei'))
      (Id.Set.inter (bv p) e_fv) 
      (p,e1) in 
  match e0 with
  | EVar(_,y) -> 
    if Id.equal x y then e
    else e0 
  | EApp(i, e1, e2) -> 
    EApp(i,subst_exp e x e1,subst_exp e x e2)

  | EFun(i,Param(_,p,t),e1) -> 
    if 
      Id.Set.mem x (bv p) then e0 
    else 
      let p',e1' = freshen p e1 in
      EFun(i,Param(i,p',t),subst_exp e x e1')

  | ECond(i,e1,e2,e3) -> 
    ECond(i,subst_exp e x e1, subst_exp e x e2, subst_exp e x e3)
  | ELet(i,Bind(_,p,t,e1),e2) ->
    let e1' = subst_exp e x e1 in 
    let p',e2' = freshen p e2 in 
    ELet (i,Bind(i,p',t,e1'),subst_exp e x e2')
  | EAsc (i,e1,t) -> 
    EAsc (i,subst_exp e x e1,t)
  | EOver (i,o,l) -> 
    EOver (i,o,Data.List.map (subst_exp e x) l)
  
  | EPair (i, e1, e2) -> 
    EPair (i,subst_exp e x e1, subst_exp e x e2)
  | ECase (i,e1,bs) ->
    let bs' = Data.List.map (fun (pi,ei) -> (pi, subst_exp e x ei)) bs in 
    ECase(i,subst_exp e x e1,bs')
  
  | EUnit(_) -> e0 
  | EInteger(_) -> e0
  | EChar(_) -> e0
  | EString(_) -> e0
  | EBool(_) -> e0

let rec ftv_pattern pat = match pat with
  | PWild(_)      -> Id.Set.empty
  | PUnit(_)      -> Id.Set.empty
  | PBool(_,_)    -> Id.Set.empty
  | PInteger(_,_) -> Id.Set.empty
  | PString(_,_)  -> Id.Set.empty
  | PVar(_,_,t_opt) -> BatOption.map_default ftv Id.Set.empty t_opt
  | PData(_,_,pat_opt) -> BatOption.map_default ftv_pattern Id.Set.empty pat_opt
  | PPair(_,p1,p2) -> Id.Set.union (ftv_pattern p1) (ftv_pattern p2)

and ftv t = match t with
  | TUnit -> Id.Set.empty
  | TBool -> Id.Set.empty
  | TInteger -> Id.Set.empty
  | TChar -> Id.Set.empty
  | TString -> Id.Set.empty

  | TProduct(t1, t2) -> Id.Set.union (ftv t1) (ftv t2)
  | TData(ts, _) ->
    List.fold_left
        (fun set t -> Id.Set.union set (ftv t))
        Id.Set.empty
        ts

  | TFunction(t1, t2) -> Id.Set.union (ftv t1) (ftv t2)
  | TVar(id) -> Id.Set.singleton id

let rec ftv_exp exp = match exp with
  | EVar(_,_) -> Id.Set.empty
  | EApp (_, e1, e2) -> Id.Set.union (ftv_exp e1) (ftv_exp e2)
  | EFun (_,Param(_,pat,t_opt),e) ->
    Id.Set.union
    (ftv_exp e)
    (Id.Set.union
        (ftv_pattern pat)
        (BatOption.map_default ftv Id.Set.empty t_opt)
    )
  | ECond (_,e1,e2,e3) -> 
    Id.Set.union (Id.Set.union (ftv_exp e1) (ftv_exp e2)) (ftv_exp e3)
  | ELet (_,Bind(_,pat,t_opt,bind_e),e) ->
    Id.Set.union
        (Id.Set.union
            (ftv_pattern pat)
            (BatOption.map_default ftv Id.Set.empty t_opt))
        (Id.Set.union
            (ftv_exp bind_e)
            (ftv_exp e))
  | EAsc (_,e,t) -> Id.Set.union (ftv_exp e) (ftv t)
  | EOver (_,_,_) -> raise UnimplementedException

  | EPair (_,e1,e2) -> Id.Set.union (ftv_exp e1) (ftv_exp e2)
  | ECase (_, m_exp, exps) ->
    Id.Set.union
        (ftv_exp m_exp)
        (List.fold_left
            (fun acc (pat, e) ->
                Id.Set.union (Id.Set.union (ftv_pattern pat) (ftv_exp e)) acc)
            Id.Set.empty exps)

  | EUnit (_)      -> Id.Set.empty
  | EInteger (_,_) -> Id.Set.empty
  | EChar (_,_)    -> Id.Set.empty
  | EString (_,_)  -> Id.Set.empty
  | EBool (_,_)    -> Id.Set.empty

let op_options = function
  | OSemi -> OpOptionSet.empty
  | OEqual -> OpOptionSet.empty
  | OMinus -> OpOptionSet.empty
  | OLt -> OpOptionSet.empty
  | OLeq -> OpOptionSet.empty
  | OGt -> OpOptionSet.empty
  | OGeq -> OpOptionSet.empty
