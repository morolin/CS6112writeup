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
(* Type checking for the Frenetic syntax                                      *)
(* $Id$ *)
(******************************************************************************)
(*TODO(astory): useful errors*)
module StrMap = Map.Make (String)
include Syntax
module TypeMap = Map.Make (
  struct
    let compare = compare
    type t = typ
   end )
module ConstraintSet = Set.Make (
  struct
    let compare = compare
    type t = typ * typ
  end )

open Util

let dummy = Info.dummy("dummy info")

exception TypeException of (Info.t * string)

let empty_scheme t = (Id.Set.empty, t)

type subst = typ Id.Map.t 
type constrt = ConstraintSet.t

let rec vars n = lazy (
    Node((dummy, None, "a" ^ (string_of_int n)), vars (n+1))
)

let idMapSingleton k v =
  Id.Map.add k v Id.Map.empty

let rec get_first_fresh set llist =
    match Lazy.force(llist) with
      | Empty -> failwith "List not infinite"
      | Node (name, l) ->
        if not (Id.Set.mem name set) then name else get_first_fresh set l

(* Generates a variable that is fresh in exp, and does not conflict with type or
regular variables *)
let fresh (info, exp) =
    let fvs = Id.Set.union (Syntax.fv exp) (Syntax.ftv_exp exp) in
    let var = get_first_fresh fvs (vars 0) in
    TVar(var)

let fv var typ =
    Id.Set.mem var (Syntax.ftv typ)

let cunion = List.fold_left (ConstraintSet.union) ConstraintSet.empty

let cadd t1 t2 = ConstraintSet.add (t1, t2)

let ceq t1 t2 = ConstraintSet.singleton (t1, t2)

let c0 = ConstraintSet.empty

let free_vars (e:exp) =
  let symbols = Syntax.fv e in
  let rec vars n = 
    let id = (dummy,None,"t"^(string_of_int n)) in
    if Id.Set.mem id symbols then vars (n+1) else
      Node(id, lazy(vars (n+1)))
  in vars 0

let lazy_get ll = 
  match Lazy.force(!ll) with
    | Empty -> failwith "List not infinite"
    | Node (i, ls) -> ll := ls; i

let dict_ftv gamma =
  let add_entry _ (Scheme(a's,t)) set =
    Id.Set.union (Id.Set.diff (Syntax.ftv t) a's) set
  in
  Id.Map.fold add_entry gamma Id.Set.empty

let rec substitute typ sigma = match typ with 
  | TUnit -> typ
  | TBool -> typ
  | TInteger -> typ
  | TChar -> typ
  | TString -> typ

  | TProduct(t1, t2) ->
  	TProduct((substitute t1 sigma),(substitute t2 sigma))
  | TData(typs, id) ->
  	TData(List.map (fun t -> substitute t sigma) typs, id)
  | TFunction(t1, t2) -> TFunction((substitute t1 sigma),(substitute t2 sigma))
  | TVar(id) ->
    if Id.Map.mem id sigma then
      Id.Map.find id sigma
    else
      typ
 
let sub_constraints sub constraints =
  ConstraintSet.fold
    (fun (t1, t2) set ->
      ConstraintSet.add ((substitute t1 sub), (substitute t2 sub)) set)
      constraints
      ConstraintSet.empty

(* Pierce, Types and Programming Languages, 2002, page 318
                   [X -> sigma(T) for each (X->T) in gamma
   sigma . gamma = [X -> T for each (X -> T) in sigma
                   [                with X not in domain (gamma)*)
let compose (sigma:subst) (gamma:subst) : subst =
  let output = Id.Map.map (fun typ -> substitute typ sigma) gamma in
  Id.Map.fold
    (fun k v acc ->
      if Id.Map.mem k gamma then 
        acc
      else
        Id.Map.add k v acc)
    sigma
    output

(* Wrapper to deal with option *)
let compose_opt (sigma_opt:subst option) (gamma:subst) : subst option =
  match sigma_opt with
  | Some sigma -> Some (compose sigma gamma)
  | None -> None

let rec unify cs =
    if ConstraintSet.is_empty cs then Some Id.Map.empty
    else
        let (s,t) = ConstraintSet.choose cs in
        let cs' = ConstraintSet.remove (s,t) cs in
        if s == t then
            unify cs'
        else match (s,t) with
          | (TVar(var), _) when not (fv var t) ->
            let sub = idMapSingleton var t in
            compose_opt (unify (sub_constraints sub cs')) sub
          | (_, TVar(var)) when not (fv var s) ->
            let sub = idMapSingleton var s in
            compose_opt (unify (sub_constraints sub cs')) sub
          | (TFunction(s1,s2),TFunction(t1,t2)) ->
            unify (cunion [cs'; ceq s1 t1; ceq s2 t2])
          | _ -> None

let unify_err cs =
  match unify cs with 
  | Some x -> x
  | None -> raise (TypeException (Info.M (""), "Could not unify"))

let rec assign_types names (gamma, delta, constraints) info pattern t alphas =
    match pattern with
      | PWild (info') -> (gamma, constraints)
      | PUnit (info') -> (gamma, cadd TUnit t constraints)
      | PBool (info', _) -> (gamma, cadd TBool t constraints)
      | PInteger (info', _) -> (gamma, cadd TInteger t constraints)
      | PString (info', _) -> (gamma, cadd TString t constraints)
      | PVar (_, id, typ_opt) ->
        (match typ_opt with
          | Some t' ->
            (Id.Map.add id (Scheme(alphas, t)) gamma, cadd t' t constraints)
          | None ->
            (Id.Map.add id (Scheme(alphas, t)) gamma, constraints))
      (*| PData (info, id, pattern) TODO *)
      | PPair (info, p1, p2) ->
        let fvs = Syntax.ftv t in
        let t1_name = get_first_fresh fvs (vars 0) in
        let fvs' = Id.Set.add t1_name fvs in
        let t2_name = get_first_fresh fvs' (vars 0) in
        let t1 = TVar(t1_name) in
        let t2 = TVar(t2_name) in
        let (gamma', constraints') =
            assign_types names (gamma, delta, constraints) info p1 t1 alphas in
        let (gamma'', constraints'') =
            assign_types names (gamma', delta, constraints') info p2 t2 alphas in
        (gamma'', cunion [constraints''; ceq t (TProduct(t1,t2))])
      | _ -> raise (TypeException(info, "PData not supported"))

let rec typecheck_exp free (gamma:scheme Id.Map.t) delta expr =
  match expr with
    | EVar (info, id) ->
      if Id.Map.mem id gamma then
        let build_sub id sub =
          (* NOTE: is this safe? *)
          let fresh = lazy_get free in (* Updates free *)
          Id.Map.add id (TVar(fresh)) sub
        in
        let Scheme(ids, t) = Id.Map.find id gamma in
        let substitution = Id.Set.fold build_sub ids Id.Map.empty in
        (substitute t substitution, c0)
      else
        raise (TypeException (info, "Unbound value " ^ (Id.string_of_t id)))
    | EApp (info, expr1, expr2) ->
      let resultant_type = fresh (info, expr) in
      let (typ1, constraints1) =
          typecheck_exp free gamma delta expr1 in
      let (typ2, constraints2) =
          typecheck_exp free gamma delta expr2 in
      let constraints' =
          cunion [
              constraints1;
              constraints2;
              ceq typ1 (TFunction (typ2, resultant_type))]
      in
      (resultant_type, constraints')
    | EFun (info, param, e) ->
      (match param with
      | Param (param_info, pattern, typ) ->
        let t1 = fresh(info, expr) in
        let constraints = (match typ with
          | Some t -> ceq t1 t
          | None -> c0) in
        let (gamma', constraints') =
          assign_types free (gamma, delta, constraints)
            (*NOTE:is the empty set of free vars the right choice here? *)
            param_info pattern t1 Id.Set.empty
        in
        let (t, constraints'') = typecheck_exp free gamma' delta e in
        (TFunction(t1, t), cunion [constraints''; constraints']))
    | ECond(i,e1,e2,e3) -> 
      let (t1, c1) = typecheck_exp free gamma delta e1 in
      let (t2, c2) = typecheck_exp free gamma delta e2 in
      let (t3, c3) = typecheck_exp free gamma delta e3 in
      let constraints' =
        cunion [
          c1; c2; c3;
          ceq t1 TBool;
          ceq t2 t3]
      in
      (t2, constraints')
    | ELet (info, bind, expr) ->
      (match bind with
        | Bind (info, pattern, typ, expr') ->
          let alphas = Id.Set.diff (Syntax.ftv_exp expr) (dict_ftv gamma) in
          let (expr'_t, constraints) = typecheck_exp free gamma delta expr' in
          let (gamma', constraints') =
                assign_types free (gamma, delta, constraints)
                    info pattern expr'_t alphas in
          typecheck_exp free gamma' delta expr)
    | EAsc (info, expr, typ) ->
      let (expr_t, constraints) = typecheck_exp free gamma delta expr in
      (expr_t, cadd expr_t typ constraints)
    | EOver (info, op, exprs) ->
       (* type check and unify all the expressions as much as possible, apply
        * substitutions
        * table full of operators and types they expect, if exactly one match,
        * use it, otherwise, barf
        *
        * Using the type might cause other types to become unified.
        * Soft matches
        *)
       raise (TypeException(info, "Overloaded operators not implemented"))

    | EPair (info, expr1, expr2) ->
      let (typ1, constraints1) =
          typecheck_exp free gamma delta expr1 in
      let (typ2, constraints2) =
          typecheck_exp free gamma delta expr2 in
      (TProduct (typ1, typ2), cunion [constraints1; constraints2])
    | ECase (info, expr1, pat_exprs) ->
      raise (TypeException(info, "Case operator not implemented"))

    | EUnit    (info)        -> (TUnit,    ConstraintSet.empty)
    | EBool    (info, value) -> (TBool,    ConstraintSet.empty)
    | EInteger (info, value) -> (TInteger, ConstraintSet.empty)
    | EChar    (info, value) -> (TChar,    ConstraintSet.empty)
    | EString  (info, value) -> (TString,  ConstraintSet.empty)

let typecheck_decl (gamma, delta, constraints) decl =
  match decl with
    | DLet (info, bind) ->
      (match bind with 
        | Bind (info, pattern, typ, expr) ->
          let free = ref (lazy (free_vars expr)) in
          let alphas = Id.Set.diff (Syntax.ftv_exp expr) (dict_ftv gamma) in
          let (expr_t, constraints) = typecheck_exp free gamma delta expr in
          let (gamma', constraints') =
                assign_types free (gamma, delta, constraints)
                    info pattern expr_t alphas in
          gamma',delta,constraints')
    | DType (info, ids, id, labels) ->
      let add_constructor g (lid, typ_opt)=
        let t = (match typ_opt with | Some x -> x | None -> TUnit) in
        let ts = List.map (fun id -> TVar(id)) ids in
        let idset = List.fold_left
            (fun set id -> Id.Set.add id set)
            Id.Set.empty
            ids in
        let scheme = Scheme(idset, TFunction(t,TData(ts,id))) in
        Id.Map.add lid scheme g
      in
      let gamma' = List.fold_left add_constructor gamma labels in
      let delta' = Id.Map.add id labels delta in
      (gamma', delta', constraints)

let typecheck_modl = function
  | Modl (info, m, ds) ->
    let gamma = Id.Map.empty in
    let delta = Id.Map.empty in
    let constraints = ConstraintSet.empty in
    let (_,_,constraints') =
        List.fold_left typecheck_decl (gamma, delta, constraints) ds in
    let _ = unify_err constraints' in
    ()
