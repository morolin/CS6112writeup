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

type subst = typ Id.Map.t
type constrt = ConstraintSet.t

module TypeListSet = Set.Make (
  struct
    let compare = compare
    type t = typ list * subst
  end )

module BindSet = Set.Make (
  struct
    let compare = compare
    type t = Id.t * typ
  end )

open Util

let dummy = Info.dummy("dummy info")

exception TypeException of (Info.t * string)

let empty_scheme t = (Id.Set.empty, t)

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

let free_vars used_symbols =
  let rec vars n =
    let id = (dummy,None,"t"^(string_of_int n)) in
    if Id.Set.mem id used_symbols then vars (n+1) else
      Node(id, lazy(vars (n+1)))
  in lazy(vars 0)

let rec type_vars = function
  | TUnit -> Id.Set.empty
  | TBool -> Id.Set.empty
  | TInteger -> Id.Set.empty
  | TChar -> Id.Set.empty
  | TString -> Id.Set.empty

  | TProduct(t1,t2) -> Id.Set.union (type_vars t1) (type_vars t2)
  | TData(ts,id) ->
    List.fold_left
      (fun s t -> Id.Set.union s (type_vars t))
      (Id.Set.singleton id)
      ts

  | TFunction(t1,t2) -> Id.Set.union (type_vars t1) (type_vars t2)
  | TVar(id) -> Id.Set.singleton id

let type_opt_vars = function
  | Some t -> type_vars t
  | None -> Id.Set.empty

let rec bind_vars = function
  | Bind(_,pat,t_opt,e) ->
    (* NOTE: I *think* that going over the pattern is unnecessary, but it
     * doesn't hurt
     *)
    Id.Set.union
      (Id.Set.union (exp_vars e) (pat_vars pat))
      (type_opt_vars t_opt)

and exp_vars = function
  | EVar (_,id) -> Id.Set.singleton id
  | EApp (_, e1,e2) -> Id.Set.union (exp_vars e1) (exp_vars e2)
  | EFun (_,Param(_,p,t_opt),e) ->
    Id.Set.union
      (Id.Set.union (pat_vars p) (exp_vars e))
      (type_opt_vars t_opt)
  | ECond (_,e1,e2,e3) ->
    Id.Set.union
      (Id.Set.union (exp_vars e1) (exp_vars e2))
      (exp_vars e3)
  | ELet (_,bind,e) -> Id.Set.union (bind_vars bind) (exp_vars e)
  | EAsc (_,e,t) -> Id.Set.union (exp_vars e) (type_vars t)
  | EOver (_,_,es) ->
    List.fold_left (fun s e -> Id.Set.union s (exp_vars e)) Id.Set.empty es

  | EPair (_,e1,e2) -> Id.Set.union (exp_vars e1) (exp_vars e2)
  | ECase (_,e,pes) ->
    List.fold_left
      (fun s (p, e) -> Id.Set.union(Id.Set.union (pat_vars p) (exp_vars e)) s)
      (exp_vars e)
      pes

  | EUnit(_)    -> Id.Set.empty
  | EInteger(_) -> Id.Set.empty
  | EChar(_)    -> Id.Set.empty
  | EString(_)  -> Id.Set.empty
  | EBool(_)    -> Id.Set.empty

and pat_vars = function
  | PWild(_) -> Id.Set.empty
  | PUnit(_) -> Id.Set.empty
  | PBool(_) -> Id.Set.empty
  | PInteger(_) -> Id.Set.empty
  | PString(_) -> Id.Set.empty
  | PVar(_,id,t_opt) ->
    Id.Set.add id (type_opt_vars t_opt)
  | PData(_,id,pat_opt) ->
    let set = (match pat_opt with
    | Some pat -> pat_vars pat
    | None -> Id.Set.empty) in
    Id.Set.add id set
  | PPair(_,p1,p2) -> Id.Set.union (pat_vars p1) (pat_vars p2)

let decl_vars = function
  | DLet (_,bind) ->
    bind_vars bind
  | DType (_, ids, id, labels) ->
    let idset =
      List.fold_left (fun s id -> Id.Set.add id s) (Id.Set.singleton id) ids in
    let add_label set (id, t_opt) =
      Id.Set.union
        (Id.Set.add id set)
        (type_opt_vars t_opt)
    in
    List.fold_left add_label idset labels

let modl_vars (Modl(_,_,ds)) =
  List.fold_left
      (fun set d -> Id.Set.union set (decl_vars d))
      Id.Set.empty
      ds

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

let create_substitution free ids =
  let build_substitution id sub =
    (* NOTE: is this safe? *)
    let fresh = lazy_get free in (* Updates free *)
    Id.Map.add id (TVar(fresh)) sub
  in
  Id.Set.fold build_substitution ids Id.Map.empty

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
          | (TData(ts1,id1), TData(ts2,id2)) ->
            if (Id.equal id1 id2)
                && (List.length (ts1) == List.length(ts2)) then
              unify
                (List.fold_left
                  (fun cs (t1, t2) -> cadd t1 t2 cs)
                  cs'
                  (List.combine ts1 ts2))
            else None
          (* TODO(astory): Do we need to add product, and primitive equality
           * too?  I bet we do :/*)
          | _ -> None

let unify_err cs =
  match unify cs with
  | Some x -> x
  | None -> raise (TypeException (Info.M (""), "Could not unify"))

(* TODO(astory): remove constraints from input *)
let rec assign_types free (gamma, delta) info pattern t =
    match pattern with
      | PWild (info') -> (BindSet.empty, c0)
      | PUnit (info') -> (BindSet.empty, ceq TUnit t)
      | PBool (info', _) -> (BindSet.empty, ceq TBool t)
      | PInteger (info', _) -> (BindSet.empty, ceq TInteger t)
      | PString (info', _) -> (BindSet.empty, ceq TString t)
      | PVar (_, id, typ_opt) ->
        (match typ_opt with
          | Some t' ->
            (BindSet.singleton (id,t), ceq t' t)
          | None ->
            (BindSet.singleton (id,t), c0))
      | PData (info, id, pat_opt) ->
        if Id.Map.mem id gamma then
          (* What we're doing:
           * we start with something like Cons(pattern).
           *
           * We first find out that Cons is of type
           *   fun(('a, 'a list) -> 'a * list)
           * and that this is predicated on 'a.
           *
           * So this tells us two things:  the pattern we expect to use as
           * input, and the resultant type.
           *
           * Next, get fresh types (here for just 'a) and substitute them in.
           *
           * After this, we know what type the pattern has to be, so recurse on
           * the input type to the function, and we know the output type to the
           * function, so we can return that.
           *
           * We also add the constraint that the resultant type for this whole
           * shebang is the output type that we found.
           *)
          let Scheme(ids, data_type) = Id.Map.find id gamma in
          (match data_type with
            | TFunction(in_t,out_t) ->
              let substitution = create_substitution free ids in
              let in_t' = substitute in_t substitution in
              let out_t' = substitute out_t substitution in
              let (bindset, constraints) =
                (match pat_opt with
                | Some(pattern) ->
                  assign_types free (gamma, delta)
                    info pattern in_t'
                | None -> (BindSet.empty, c0)
                ) in
              (bindset, cunion [constraints; ceq t out_t'])
            | _ ->
              Error.simple_error
                ("Type constructor "^(Id.string_of_t id)^" not a function")
          )
        else
          raise (TypeException(info, ("Unknown data type" ^ (Id.string_of_t id))))
      | PPair (info, p1, p2) ->
        let t1 = TVar(lazy_get free) in
        let t2 = TVar(lazy_get free) in
        let (bs1, constraints1) =
            assign_types free (gamma, delta) info p1 t1 in
        let (bs2, constraints2) =
            assign_types free (gamma, delta) info p2 t2 in
        (BindSet.union bs1 bs2,
          cunion [constraints1; constraints2; ceq t (TProduct(t1,t2))])

let rec typecheck_exp free (gamma:scheme Id.Map.t) delta expr =
  match expr with
    | EVar (info, id) ->
      if Id.Map.mem id gamma then
        let Scheme(ids, t) = Id.Map.find id gamma in
        let substitution = create_substitution free ids in
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
        let (bind_set, constraints') =
          assign_types free (gamma, delta) param_info pattern t1
        in
        (* Bind with the empty type scheme because we're not generalizing types
         *)
        let add_binding (id,t) gamma =
          Id.Map.add id (Scheme(Id.Set.empty, t)) gamma
        in
        let gamma' = BindSet.fold add_binding bind_set gamma in
        let (t, constraints'') = typecheck_exp free gamma' delta e in
        (TFunction(t1, t), cunion [constraints; constraints'; constraints'']))
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
    | ELet (_, Bind (info, pattern, typ, expr'), expr) ->
      let (gamma', constraints') =
        check_let free gamma delta info pattern typ expr' in
      let (t, constraints'') = typecheck_exp free gamma' delta expr in
      (t, cunion [constraints'; constraints''])
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
      let types_options = [] in (* TODO(astory): this needs to be a real lookup *)
      let real_name = TVar(dummy, None, "??") in
      let checked =
        List.map (fun e -> typecheck_exp free gamma delta e) exprs in
      let build_matches set (types:typ list) =
        let build_check constraints (t1, (t2, cs)) =
          cunion [constraints; cs; ceq t1 t2]
        in
        let cs = List.fold_left build_check c0 (List.combine types checked) in
        (match unify cs with
        (* If it unifies, we keep it, otherwise, throw it out *)
        | Some subst -> TypeListSet.add (types, subst) set
        | None -> set)
      in
      let matches =
        List.fold_left build_matches TypeListSet.empty types_options in
      (match TypeListSet.cardinal matches with
      | 0 -> raise
        (TypeException(info, "No matches for overloaded operator"))
      | 1 ->
        let (types, subst) = TypeListSet.choose matches in
        (* These types are as specific as they're going to get TODO right? *)
        let types' = List.map (fun t -> substitute t subst) types in
        (* TODO: leave for now, but we need to have a way to change the
         * expression going up *)
        let build_type t1 t2 = TFunction(t1, t2) in
        (List.fold_left build_type real_name types', c0)
      | _ -> raise
        (TypeException(info, "Too many matches for overloaded operator"))
      )

    | EPair (info, expr1, expr2) ->
      let (typ1, constraints1) =
          typecheck_exp free gamma delta expr1 in
      let (typ2, constraints2) =
          typecheck_exp free gamma delta expr2 in
      (TProduct (typ1, typ2), cunion [constraints1; constraints2])
    | ECase (info, expr1, pat_exprs) ->
      (* TODO(astory): verify *)
      let output_t = TVar(lazy_get free) in
      let build_check (t, cs) (p, e) =
        let (gamma', cs') = check_let free gamma delta info p None expr1 in
        let (t', cs'') = typecheck_exp free gamma' delta e in
        (t, cunion [cs; cs'; cs''; ceq t t'])
      in
      List.fold_left build_check (output_t, c0) pat_exprs

    | EUnit    (info)        -> (TUnit,    ConstraintSet.empty)
    | EBool    (info, value) -> (TBool,    ConstraintSet.empty)
    | EInteger (info, value) -> (TInteger, ConstraintSet.empty)
    | EChar    (info, value) -> (TChar,    ConstraintSet.empty)
    | EString  (info, value) -> (TString,  ConstraintSet.empty)

and check_let free gamma delta info pat eq_t expr =
  (* Check bound expression, and calculate bindings and constraints resulting
   * from its binding *)
  let (expr_t, constraints) = typecheck_exp free gamma delta expr in
  let (bind_set, constraints') =
    assign_types free (gamma, delta) info pat expr_t in
  (* If a type annotation is specified, enforce it *)
  let constraints'' = match eq_t with
    | Some t -> ceq t expr_t
    | None -> c0 in
  (* Solve constraints, and for each binding substitute, generalize, add to
   * gamma.  A failure to unify is fatal, so fail hard. *)
  let sub = unify_err (cunion [constraints; constraints'; constraints'']) in
  let add_binding (id,t) gamma =
    let t' = substitute t sub in
    let alphas = Id.Set.diff (Syntax.ftv t') (dict_ftv gamma) in
    Id.Map.add id (Scheme(alphas, t')) gamma
  in
  let gamma' = BindSet.fold add_binding bind_set gamma in
  (gamma', constraints'')

let typecheck_decl free (gamma, delta, constraints) decl =
  match decl with
    | DLet (info, bind) ->
      (match bind with
        | Bind (info, pattern, typ, expr) ->
          let (gamma', constraints') =
            check_let free gamma delta info pattern typ expr in
          gamma', delta, cunion [constraints; constraints'])
    | DType (info, ids, id, labels) ->
      let ts = List.map (fun id -> TVar(id)) ids in
      let idset = List.fold_left
        (fun set id -> Id.Set.add id set)
        Id.Set.empty
        ids in
      let add_constructor g (lid, typ_opt)=
        let t = (match typ_opt with | Some x -> x | None -> TUnit) in
        let scheme = Scheme(idset, TFunction(t,TData(ts,id))) in
        Id.Map.add lid scheme g  
      in
      let gamma' = List.fold_left add_constructor gamma labels in
      let delta' = Id.Map.add id labels delta in
      (gamma', delta', constraints)

let typecheck_modl modl =
  let used_symbols = modl_vars modl in
  let free = ref (free_vars used_symbols) in
  match modl with
  | Modl (info, m, ds) ->
    let gamma = Id.Map.empty in
    let delta = Id.Map.empty in
    let constraints = ConstraintSet.empty in
    let (_,_,constraints') =
        List.fold_left (typecheck_decl free) (gamma, delta, constraints) ds in
    let _ = unify_err constraints' in
    ()
