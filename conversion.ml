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
(* /src/compiler/conversion.ml                                                *)
(* AST manipulations                                                          *)
(* $Id$ *)
(******************************************************************************)

module StrMap = Map.Make(String)

open Syntax
open Printf

let dummy = Info.dummy("dummy info")

let fresh_cell = ref 0 

(* poor man's gensym; needs to be repleaced with a real one *)
let get_fresh s =
  incr fresh_cell;
  s ^ (string_of_int !fresh_cell)

let fresh () = (dummy, None, get_fresh "f_")

let undersc () = (dummy, None, get_fresh "_")

let make_function i pat e =
  EFun(i, Param(dummy,pat,None),e)

let make_var i z =
  EVar(i, z)

let simple_var = make_var dummy

let simple_var_id name =
  make_var dummy (dummy, None, name)

let make_pvar i z =
  PVar(i, z, None)

let make_application i name e =
  EApp(i, e, make_var i name) 

let simple_app e1 e2=
    EApp(dummy, e1, e2)

(* functions in prelude, to be moved to some other place in the future *)
(* TODO(astory): move me! *)
let globals = List.fold_left
  (fun map (k, v) -> StrMap.add k v map)
  StrMap.empty
  [
  ("_match_failure", simple_var_id "_match_failure");
  ("land", simple_var_id "land");
  ("_equals", simple_var_id "_equals");
  ("_is_pair", simple_var_id "_is_pair");
  ("_isinstance", simple_var_id "_isinstance");
  ("_value", simple_var_id "_value");
  ]

let g_find name = StrMap.find name globals

let match_exception(info) =
    EApp(info,
      g_find "_match_failure",
      EString(dummy,
        String.escaped (sprintf "Match_failure %s" (Info.string_of_t info)))
    )

let make_and e1 e2 =
    simple_app (simple_app(g_find "land") e1) e2

let make_instance e1 e2 =
    simple_app (simple_app(g_find "_isinstance") e1) e2

let make_equals (e1:exp) (e2:exp) =
    simple_app
      (simple_app (g_find "_equals") e1)
      e2

(* end of such functions *) 

let rec has_data (pat:pattern) =
  match pat with
  | PData(_) -> true
  | PPair(_,p1, p2) -> (has_data p1) || (has_data p2)
  | _ -> false

let rec make_match (pat:pattern) (e:exp) =
    match pat with
    | PWild(_) -> EBool(dummy, true)
    | PUnit(_) -> make_equals e (EUnit(dummy))
    | PBool(_, value) -> make_equals e (EBool(dummy, value))
    | PInteger(_, value) -> make_equals e (EInteger(dummy, value))
    | PString(_, value) -> make_equals e (EString(dummy, value))
    | PVar(_) -> EBool(dummy, false)
    | PData(_, id, pat_opt) -> 
      (* logic:  check that e is of the same type as name, and then recurse with
      pattern on e.value *)
      let safe = make_instance (simple_var id) e in
      (match pat_opt with
      | Some pat ->
        make_and
          safe
          (make_match pat (simple_app (g_find "_value") e))
      | None -> safe)
    | PPair(_, p1, p2) ->
      (* Logic:
       * Test if e is a pair.  If it isn't, return false.  If it is, then it's
       * safe to use a Let statement to split it into its two halves, and then
       * recursively check the two sub-{pattern,expression}s.
       *)
      let e1_id = undersc() in
      let e2_id = undersc() in
      make_and
        (simple_app (g_find "_is_pair") e)
        (ELet(
          dummy,
          Bind(dummy,
            PPair(dummy, make_pvar dummy e1_id, make_pvar dummy e2_id),
            None,
            e),
          make_and
            (make_match p1 (simple_var e1_id))
              (make_match p2 (simple_var e2_id))
          ))

(* Recurse through a pattern, find all the data types and replace them with
 * variables, and keep track of everything you did this for, with enough data to
 * re-constitute it, i.e., a list of varname * PData(...).
 *)
let rec extract_data:
      pattern -> pattern * (Id.t * pattern) list =
  fun pat -> match pat with
  | PWild(_)
  | PUnit(_)
  | PBool(_)
  | PInteger(_)
  | PString(_)
  | PVar(_) -> (pat, [])
  | PData(info, id, pat_opt) ->
    let var = undersc() in
    let p = make_pvar info var in
    (p, [(var, pat)])
  | PPair(info, p1, p2) ->
    let new1, vars1 = extract_data p1 in
    let new2, vars2 = extract_data p2 in
    (PPair(info, new1, new2), vars1 @ vars2)

let wrap_data e (name, pat) =
  match pat with
  | PData(info, did, pat_opt) ->
    (* Assumption: name is defined in scope with the data member
     * that should match our example.
     *)
    let var = simple_var name in
    let unpack = (match pat_opt with
      | Some(pattern) ->
        simple_app
          (EFun(dummy, Param(dummy, pattern, None), e))
          (simple_app (g_find "_value") var)
      | None -> e) in
    ECond(dummy,
      make_instance (simple_var did) (simple_var name),
      unpack,
      match_exception(info))
  | _ -> Error.simple_error "Got non-data"

let rec convert_exp (top:bool) (vs:Id.Set.t) (e:exp) = 
  match e with
    | EVar(_) -> 
      (e, [])
    | EApp(i,e1,e2) ->
      let e1',ds1' = convert_exp false vs e1 in 
      let e2',ds2' = convert_exp false vs e2 in 
      (EApp(i, e1', e2'), ds1' @ ds2')
    | EFun(i,Param(pi,p,pto),e1) ->
      if (has_data p) then
        let (clean_pat, datas) = extract_data p in (* clean_pat has no data *)
        let exp = List.fold_left wrap_data e1 datas in
        convert_exp top vs (EFun(i, Param(pi,clean_pat,pto), exp))
      else
        let e1', ds1' = convert_exp top vs e1 in 
        let f1 = make_function i p e1' in 
        if top then (f1,ds1')
        else 
          let h = fresh () in
          let zs = 
            Id.Set.elements 
              (Id.Set.diff (fv e1') 
                 (Id.Set.union 
                    (Data.List.fold_left (fun vs (f,_) -> Id.Set.add f vs) vs ds1')
                    (bv p))) in 
          let f = 
            List.fold_left 
              (fun f z -> make_function i (make_pvar dummy z) f)
              f1 zs in 
          let e' = 
            List.fold_right 
              (make_application i) 
              zs (make_var i h) in
          (e', (h,f) :: ds1') 
    | ECond(i,e1,e2,e3) -> 
      let e1',ds1' = convert_exp false vs e1 in 
      let e2',ds2' = convert_exp false vs e2 in 
      let e3',ds3' = convert_exp false vs e3 in 
      (ECond(i,e1',e2',e3'),ds1' @ ds2' @ ds3')
    | ELet (i,Bind(_,pat,typ,l_exp),exp) ->
      convert_exp false vs (EApp(i,EFun(i,Param(i,pat,typ),exp),l_exp))
    | EAsc(_,e1,_) -> convert_exp top vs e1
    | EOver(_,_,_) ->
      Error.simple_error "Overloaded operator in AST to convert" 
        
    | EPair(i,e1,e2) ->
      let e1',ds1' = convert_exp false vs e1 in 
      let e2',ds2' = convert_exp false vs e2 in 
      (EPair(i,e1',e2'),ds1' @ ds2')
    | ECase (i,e,es) ->
        let e_name = undersc() in
        let e_pat = make_pvar dummy e_name in
        let e_var = make_var dummy e_name in
        let fold_case (pat, e) else_e =
            let pat_sub_e = ELet(dummy, Bind(dummy, pat, None, e_var), e) in
            print_endline (string_of_bool (has_data pat));
            ECond(dummy, make_match pat e_var, pat_sub_e, else_e)
        in
        let expr =
            ELet(i, Bind(dummy, e_pat, None, e),
                List.fold_right fold_case es (match_exception i)
            ) in
        convert_exp false vs expr
        
    | EUnit(_) | EInteger(_) | EChar(_) | EString (_) | EBool (_) -> 
      (e,[]) 

let mk_decl i f e = 
  DLet(i,Bind(dummy,PVar(dummy,f, None), None, e))

let rec convert_decl (d:decl) (vs:Id.Set.t) = 
  match d with
    | DLet (i, Bind(bi,p,bto,e)) -> 
      if (has_data p) then
        let fresh_var = undersc() in
        let exp = ECase(dummy, simple_var fresh_var, [(p,e)]) in
        let d = DLet (i, Bind(bi, make_pvar dummy fresh_var, bto, exp)) in
        convert_decl d vs
      else
        let vs' = match p with 
          | PVar(_,x,_) -> Id.Set.add x vs 
          | _ -> vs in 
        let e', ds' = convert_exp true vs' e in
        let ds'' = 
          List.fold_left
            (fun decls (h,f) -> 
              (mk_decl (Syntax.info_of_exp f) h f)::decls)
            [DLet(i, Bind(dummy, p, None, e'))]
            ds' in 
        (ds'',vs')
    | DType(_) -> ([d], vs)

let convert_decls (ds:decl list) =
  List.fold_left 
    (fun (decls,vs) d -> 
      let decls',vs' = convert_decl d vs in 
      (decls @ decls',vs'))
    ([],Id.Set.empty) ds

let convert_module (Modl(i,m,decls)) =
  let decls',_ = convert_decls decls in 
  Modl(i,m,decls')
