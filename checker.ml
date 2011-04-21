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
module StringMap = Map.Make (String)
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


exception TypeException of (Info.t * string)

type gammat = typ StringMap.t 
type subst = gammat
type constrt = ConstraintSet.t

let rec vars n = lazy (
    Node("a" ^ (string_of_int n), vars (n+1))
)

let stringMapSingleton k v =
  StringMap.add k v StringMap.empty

let rec get_first_fresh set llist =
    match Lazy.force(llist) with
      | Empty -> failwith "List not infinite"
      | Node (name, l) ->
        if not (StrSet.mem name set) then name else get_first_fresh set l

(* Generates a variable that is fresh in exp, and does not conflict with type or
regular variables *)
let fresh (info, exp) =
    let fvs = StrSet.union (Syntax.fv exp) (Syntax.ftv_exp exp) in
    let var = get_first_fresh fvs (vars 0) in
    TVar(info, None, var)

let fv var typ =
    StrSet.mem var (Syntax.ftv typ)

let cunion = List.fold_left (ConstraintSet.union) ConstraintSet.empty

let cadd t1 t2 = ConstraintSet.add (t1, t2)

let ceq t1 t2 = ConstraintSet.singleton (t1, t2)

let c0 = ConstraintSet.empty

let rec substitute typ sigma = match typ with 
  | TUnit -> typ
  | TBool -> typ
  | TInteger -> typ
  | TChar -> typ
  | TString -> typ

  | TProduct(t1, t2) -> TProduct((substitute t1 sigma),(substitute t2 sigma))
  | TData(typs, id) -> failwith "unimplemented" (*TODO:help*)

  | TFunction(t1, t2) -> TFunction((substitute t1 sigma),(substitute t2 sigma))
  | TVar((_,_,name)) ->
    if StringMap.mem name sigma then
      StringMap.find name sigma
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
  let output = StringMap.map (fun typ -> substitute typ sigma) gamma in
  StringMap.fold
    (fun k v acc ->
      if StringMap.mem k gamma then 
        acc
      else
        StringMap.add k v acc)
    sigma
    output

let rec unify cs =
    if ConstraintSet.is_empty cs then StringMap.empty
    else
        let (s,t) = ConstraintSet.choose cs in
        let cs' = ConstraintSet.remove (s,t) cs in
        if s == t then
            unify cs'
        else match (s,t) with
          | (TVar(_,_,var), _) when not (fv var t) ->
            let sub = stringMapSingleton var t in
            compose (unify (sub_constraints sub cs')) sub
          | (_, TVar(_,_,var)) when not (fv var s) ->
            let sub = stringMapSingleton var s in
            compose (unify (sub_constraints sub cs')) sub
          | (TFunction(s1,s2),TFunction(t1,t2)) ->
            unify (cunion [cs'; ceq s1 t1; ceq s2 t2])
          | _ -> raise (TypeException (Info.M (""), "Could not unify"))

let rec assign_types (gamma, constraints) info pattern t =
    match pattern with
      | PWild (info') -> (gamma, constraints)
      | PUnit (info') -> (gamma, cadd TUnit t constraints)
      | PBool (info', _) -> (gamma, cadd TBool t constraints)
      | PInteger (info', _) -> (gamma, cadd TInteger t constraints)
      | PString (info', _) -> (gamma, cadd TString t constraints)
      | PVar (_, (info', mo, s), typ_opt) ->
        (match typ_opt with
          | Some t' -> (StringMap.add s t gamma, cadd t' t constraints)
          | None -> (StringMap.add s t gamma, constraints))
      (*| PData (info, id, pattern) *)
      | PPair (info, p1, p2) ->
          (match t with
            | TProduct(t1, t2) ->
                let (gamma', constraints') =
                    assign_types (gamma, constraints) info p1 t1 in
                assign_types (gamma', constraints') info p2 t2
            | _ -> raise (TypeException(info, "type is not a product")))
      | _ -> raise (TypeException(info, "PData not supported"))


(* for now, just return type of underlying expression.  Later, need to modify ast*)
let rec typecheck_exp gamma expr =
  match expr with
    | EVar (info, id) ->
      let s = Id.string_of_t id in
      if StringMap.mem s gamma then
        (StringMap.find s gamma, c0)
      else
        raise (TypeException (info, "Unbound value " ^ s))
    | EApp (info, expr1, expr2) ->
      let resultant_type = fresh (info, expr) in
      let (typ1, constraints1) =
          typecheck_exp gamma expr1 in
      let (typ2, constraints2) =
          typecheck_exp gamma expr2 in
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
                (* TODO(astory): make work for pairs without explicitness *)
                assign_types (gamma, constraints) param_info pattern t1
            in
            let (t, constraints'') = typecheck_exp gamma' e in
            (TFunction(t1, t), cunion [constraints''; constraints']))
    | ECond(i,e1,e2,e3) -> 
      Error.simple_error "unimplemented"
    | ELet (info, bind, expr) ->
      (match bind with
        | Bind (info, pattern, typ, expr') ->
          let (expr'_t, constraints) = typecheck_exp gamma expr' in
          let (gamma', constraints') =
                assign_types (gamma, constraints) info pattern expr'_t in
          typecheck_exp gamma' expr)
    | EAsc (info, expr, typ) ->
      let (expr_t, constraints) = typecheck_exp gamma expr in
      (expr_t, cadd expr_t typ constraints)
    | EOver (info, op, exprs) ->
        raise (TypeException(info, "Overloaded operators not implemented"))

    | EPair (info, expr1, expr2) ->
      let (typ1, constraints1) =
          typecheck_exp gamma expr1 in
      let (typ2, constraints2) =
          typecheck_exp gamma expr2 in
      (TProduct (typ1, typ2), cunion [constraints1; constraints2])
    | ECase (info, expr1, pat_exprs) ->
      raise (TypeException(info, "Case operator not implemented"))

    | EUnit    (info)        -> (TUnit,    ConstraintSet.empty)
    | EBool    (info, value) -> (TBool,    ConstraintSet.empty)
    | EInteger (info, value) -> (TInteger, ConstraintSet.empty)
    | EChar    (info, value) -> (TChar,    ConstraintSet.empty)
    | EString  (info, value) -> (TString,  ConstraintSet.empty)

let typecheck_decl (gamma, constraints) decl =
  match decl with
    | DLet (info, bind) ->
      (match bind with 
        | Bind (info, pattern, typopt, exp) ->
          let (t, constraints) = typecheck_exp gamma exp in
          assign_types (gamma, constraints) info pattern t)
    | DType (info, ids, id, labels) ->
      (*TODO(astory)*)
      (gamma, constraints)

let typecheck_modl = function
  | Modl (info, m, ds) ->
    let gamma = StringMap.empty in
    let constraints = ConstraintSet.empty in
    let (_, constraints') =
        List.fold_left typecheck_decl (gamma, constraints) ds in
    let _ = unify constraints' in
    ()
